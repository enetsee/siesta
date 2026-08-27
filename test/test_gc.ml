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
        ; Alcotest.test_case "dedup holds without GC" `Quick test_no_gc_dedup
        ; Alcotest.test_case
            "clear preserves tag uniqueness"
            `Quick
            test_clear_tag_uniqueness
        ] )
    ]
;;
