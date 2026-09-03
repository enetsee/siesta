(* GC-interaction tests for [Siesta.Dedup].

   The tables hold their entries weakly, so three things have to hold:
   collection really happens once the references are dropped, repeated
   build/drop/GC cycles leave the cache usable, and dedup still works while
   entries are alive.

   Weak-table behaviour follows GC heuristics, so the tolerances here are loose
   on purpose. *)

(* -- collection happens ---------------------------------------------------- *)

let test_weak_collection () =
  let t = Siesta.Dedup.token_create () in
  let n = 10_000 in
  (* Insert n unique tokens and let the references go out of scope. *)
  let _ =
    let tokens =
      Array.init n (fun i -> Siesta.Dedup.token_intern t ~kind:0 ~text:(string_of_int i))
    in
    let _, entries_before, _, _, _, _ = Siesta.Dedup.token_stats t in
    Printf.printf "  before drop: entries=%d\n" entries_before;
    Array.length tokens (* keep alive until here *)
  in
  (* Force a few major collections to reap them. GC heuristics let a handful
     linger, so the bar is 5% surviving. *)
  for _ = 1 to 5 do
    Gc.full_major ()
  done;
  let _, entries_after, _, _, _, _ = Siesta.Dedup.token_stats t in
  Printf.printf "  after Gc.full_major × 5: entries=%d\n" entries_after;
  Alcotest.(check bool)
    (Printf.sprintf
       "weak collection reaped ≥ 95%% of dropped entries (%d -> %d)"
       n
       entries_after)
    true
    (entries_after <= n / 20)
;;

(* -- no crashes under churn ------------------------------------------------ *)

let test_churn () =
  let t = Siesta.Dedup.token_create () in
  let nt = Siesta.Dedup.node_create () in
  let iterations = 1000 in
  for i = 0 to iterations - 1 do
    let _ =
      let toks =
        Array.init 100 (fun j ->
          Siesta.Dedup.token_intern t ~kind:(j mod 5) ~text:(Printf.sprintf "i%d_t%d" i j))
      in
      let _root =
        Siesta.Dedup.node_intern
          nt
          ~kind:1
          ~text_len:100
          ~payload:0
          (Array.map (fun t -> Siesta.Dedup.Token t) toks)
      in
      ()
    in
    if i mod 100 = 0 then Gc.full_major ()
  done;
  (* The cache has to still intern correctly after all that. *)
  let final = Siesta.Dedup.token_intern t ~kind:0 ~text:"sentinel" in
  let again = Siesta.Dedup.token_intern t ~kind:0 ~text:"sentinel" in
  Helpers.same
    (Printf.sprintf
       "churn: %d iterations × 100 nodes/each, cache still functional"
       iterations)
    final
    again
;;

(* -- the table sizes itself to the live set, not the running total --------- *)

(* An editor re-parsing on every keystroke interns a fresh batch each round and
   keeps almost none of it. The bucket array has to size itself to what is still
   alive; sizing it to the total interned makes it grow for the life of the
   session, since every round's misses push the insert counter past the resize
   threshold again. [rehash] therefore counts the survivors before it decides to
   widen.

   The bar is a ratio rather than an absolute width: how many buckets a given
   live set wants depends on how much the GC has reaped by the time a rehash
   lands, which is a heuristic. Growth in the total interned is not. *)
let test_churn_bounded_buckets () =
  let t = Siesta.Dedup.token_create () in
  let rounds = 12 in
  let per_round = 20_000 in
  (* Measured a quarter of the way in, so the remaining rounds quadruple the
     total interned. Width that tracks the total doubles twice over that. *)
  let baseline_round = 3 in
  let baseline = ref 0 in
  let kept = ref [] in
  let final_live = ref 0 in
  let final_buckets = ref 0 in
  for r = 1 to rounds do
    for j = 0 to per_round - 1 do
      ignore (Siesta.Dedup.token_intern t ~kind:0 ~text:(Printf.sprintf "r%d_%d" r j))
    done;
    (* One survivor per round, as a re-parse keeps the tokens that did not
       change. *)
    kept := Siesta.Dedup.token_intern t ~kind:1 ~text:(Printf.sprintf "keep%d" r) :: !kept;
    Gc.full_major ();
    let n, live, slots, _, _, _ = Siesta.Dedup.token_stats t in
    Printf.printf
      "  round %2d: interned=%7d buckets=%6d live=%5d slots=%7d\n"
      r
      (r * per_round)
      n
      live
      slots;
    if r = baseline_round then baseline := n;
    final_live := live;
    final_buckets := n
  done;
  (* The premise of the bound below: the round's batch really is dead by now, so
     a table that kept growing would be growing for nothing. Same 5% tolerance
     as [test_weak_collection]. *)
  Alcotest.(check bool)
    (Printf.sprintf "churn: batch reaped, %d live after %d rounds" !final_live rounds)
    true
    (!final_live <= List.length !kept + (per_round / 20));
  Alcotest.(check bool)
    (Printf.sprintf
       "churn: bucket array tracks the live set (%d buckets at %d interns, %d at %d)"
       !baseline
       (baseline_round * per_round)
       !final_buckets
       (rounds * per_round))
    true
    (!final_buckets <= 2 * !baseline);
  (* Keep the survivors alive to here, or the GC is free to reap them mid-run
     and the live count above stops meaning anything. *)
  ignore (Sys.opaque_identity !kept)
;;

(* -- deduplication holds when refs survive --------------------------------- *)

(* Two builds of the same shape in one cache, with nothing collected in
   between, have to return the same record. That is the guarantee an editor
   re-parsing on every keystroke leans on for its O(1) "did this subtree
   change". *)
let test_no_gc_dedup () =
  let nt = Siesta.Dedup.node_create () in
  (* Held in scope, so nothing can be collected under us. *)
  let leaf = Siesta.Dedup.node_intern nt ~kind:0 ~text_len:0 ~payload:0 [||] in
  let outer1 =
    Siesta.Dedup.node_intern
      nt
      ~kind:1
      ~text_len:0
      ~payload:0
      [| Siesta.Dedup.Node leaf |]
  in
  let outer2 =
    Siesta.Dedup.node_intern
      nt
      ~kind:1
      ~text_len:0
      ~payload:0
      [| Siesta.Dedup.Node leaf |]
  in
  Helpers.same "no-GC dedup: rebuilt outer is physically equal" outer1 outer2;
  Siesta.Dedup.(
    Alcotest.(check int) "no-GC dedup: tags equal" outer1.nd_tag outer2.nd_tag)
;;

(* -- cache survives a clear (Hashconsed mode) ------------------------------ *)

(* [clear] drops the entries, but the tag counter is global and never resets,
   so a tag handed out before a clear can never come round again after one. *)
let test_clear_tag_uniqueness () =
  let t = Siesta.Dedup.token_create () in
  let before = Siesta.Dedup.token_intern t ~kind:0 ~text:"hello" in
  let tag_before = Siesta.Dedup.(before.tk_tag) in
  Siesta.Dedup.token_clear t;
  let after = Siesta.Dedup.token_intern t ~kind:0 ~text:"hello" in
  Alcotest.(check bool)
    (Printf.sprintf
       "clear: post-clear intern gets fresh tag (was %d, now %d)"
       tag_before
       Siesta.Dedup.(after.tk_tag))
    true
    Siesta.Dedup.(after.tk_tag > tag_before);
  Helpers.distinct
    "clear: post-clear value is not physically equal to pre-clear"
    before
    after
;;

let () =
  Alcotest.run
    "gc"
    [ ( "weak-table"
      , [ Alcotest.test_case "collection happens" `Quick test_weak_collection
        ; Alcotest.test_case "no crash under churn" `Quick test_churn
        ; Alcotest.test_case
            "churn leaves the bucket array bounded"
            `Quick
            test_churn_bounded_buckets
        ; Alcotest.test_case "dedup holds without GC" `Quick test_no_gc_dedup
        ; Alcotest.test_case
            "clear preserves tag uniqueness"
            `Quick
            test_clear_tag_uniqueness
        ] )
    ]
;;
