(* Unit tests driving [Siesta.Dedup]'s tables directly, below the public
   Builder/Green surface that test_siesta.ml goes through. A regression in the
   table machinery then shows up as a small focused failure. *)

(* -- idempotence + distinctness (token) ------------------------------------ *)

let test_token_idempotence () =
  let t = Siesta.Dedup.token_create () in
  let a = Siesta.Dedup.token_intern t ~kind:1 ~text:"hello" in
  let b = Siesta.Dedup.token_intern t ~kind:1 ~text:"hello" in
  Helpers.same "intern x twice → physically equal" a b;
  Siesta.Dedup.(Alcotest.(check int) "same tag for equal inserts" a.tk_tag b.tk_tag);
  Alcotest.(check Testable.dedup_token) "interned records equal" a b
;;

let test_token_distinctness () =
  let t = Siesta.Dedup.token_create () in
  let a = Siesta.Dedup.token_intern t ~kind:1 ~text:"a" in
  let b = Siesta.Dedup.token_intern t ~kind:1 ~text:"b" in
  let c = Siesta.Dedup.token_intern t ~kind:2 ~text:"a" in
  Alcotest.(check bool)
    "text differs → distinct tags"
    true
    Siesta.Dedup.(a.tk_tag <> b.tk_tag);
  Alcotest.(check bool)
    "kind differs → distinct tags"
    true
    Siesta.Dedup.(a.tk_tag <> c.tk_tag);
  Helpers.distinct "distinct token not shared (text)" a b;
  Helpers.distinct "distinct token not shared (kind)" a c
;;

(* -- idempotence + distinctness (node) ------------------------------------- *)

let test_node_idempotence () =
  let t = Siesta.Dedup.token_create () in
  let nt = Siesta.Dedup.node_create () in
  let tok = Siesta.Dedup.token_intern t ~kind:0 ~text:"x" in
  let cs = [| Siesta.Dedup.Token tok |] in
  let a = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:1 ~payload:0 cs in
  let b = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:1 ~payload:0 cs in
  Helpers.same "intern same shape twice → physically equal" a b;
  Siesta.Dedup.(Alcotest.(check int) "same tag" a.nd_tag b.nd_tag)
;;

let test_node_distinctness () =
  let nt = Siesta.Dedup.node_create () in
  let a = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:0 ~payload:0 [||] in
  let b = Siesta.Dedup.node_intern nt ~kind:2 ~text_len:0 ~payload:0 [||] in
  let c = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:5 ~payload:0 [||] in
  let d = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:0 ~payload:1 [||] in
  Alcotest.(check bool) "kind differs → distinct" true Siesta.Dedup.(a.nd_tag <> b.nd_tag);
  Alcotest.(check bool)
    "text_len differs → distinct"
    true
    Siesta.Dedup.(a.nd_tag <> c.nd_tag);
  Alcotest.(check bool)
    "payload differs → distinct"
    true
    Siesta.Dedup.(a.nd_tag <> d.nd_tag)
;;

(* -- tag monotonicity / uniqueness ----------------------------------------- *)

let test_tag_monotonicity () =
  let t = Siesta.Dedup.token_create () in
  let n = 100_000 in
  let seen = Hashtbl.create n in
  let max_tag = ref 0 in
  for i = 0 to n - 1 do
    let tok = Siesta.Dedup.token_intern t ~kind:(i mod 50) ~text:(string_of_int i) in
    Siesta.Dedup.(
      if Hashtbl.mem seen tok.tk_tag then Alcotest.failf "duplicate tag %d" tok.tk_tag;
      Hashtbl.add seen tok.tk_tag ();
      if tok.tk_tag > !max_tag then max_tag := tok.tk_tag)
  done;
  Alcotest.(check int)
    (Printf.sprintf "%d distinct tags (max=%d)" n !max_tag)
    n
    (Hashtbl.length seen)
;;

(* -- hash collisions ------------------------------------------------------- *)

(* A small table plus 50 distinct tokens guarantees collisions, so every token
   has to be retrievable with its original tag through a shared bucket. *)
let test_collision () =
  let t = Siesta.Dedup.token_create ~capacity:7 () in
  let inserts =
    Array.init 50 (fun i ->
      let kind = i mod 10 in
      let text = "tok_" ^ string_of_int i in
      let tok = Siesta.Dedup.token_intern t ~kind ~text in
      tok, kind, text)
  in
  Array.iter
    (fun (orig, kind, text) ->
       let again = Siesta.Dedup.token_intern t ~kind ~text in
       Helpers.same (Printf.sprintf "collision retrieval for %S" text) orig again)
    inserts
;;

(* -- resize correctness ---------------------------------------------------- *)

let test_resize () =
  (* Default capacity is 256 and the load factor is 2, so a resize fires around
     512 entries. 5000 forces several rounds, and the sampled entries have to
     come back through all of them physically intact. *)
  let t = Siesta.Dedup.token_create () in
  let n = 5000 in
  let originals =
    Array.init n (fun i -> Siesta.Dedup.token_intern t ~kind:0 ~text:(string_of_int i))
  in
  let sample_indices = [ 0; 100; 500; 1000; 2500; 4500; 4999 ] in
  List.iter
    (fun idx ->
       let again = Siesta.Dedup.token_intern t ~kind:0 ~text:(string_of_int idx) in
       Helpers.same
         (Printf.sprintf "resize: original at %d survives" idx)
         originals.(idx)
         again)
    sample_indices
;;

(* -- empty edges ----------------------------------------------------------- *)

let test_empty_token () =
  let t = Siesta.Dedup.token_create () in
  let empty = Siesta.Dedup.token_intern t ~kind:0 ~text:"" in
  let again = Siesta.Dedup.token_intern t ~kind:0 ~text:"" in
  Helpers.same "empty token text dedupes" empty again
;;

(* [node_intern] copies the children it is handed, so a caller may reuse the
   buffer it built them in. Retaining the array instead leaves a later write
   rewriting a built node in place; [nd_text_len] drifts from the children, and
   the entry sits in a bucket its hash no longer indexes, so the shape is lost
   to every future intern. *)
let test_node_intern_copies_children () =
  let open Siesta.Dedup in
  let t = token_create () in
  let nt = node_create () in
  let a = token_intern t ~kind:0 ~text:"aaa" in
  let b = token_intern t ~kind:0 ~text:"b" in
  let cs = [| Token a; Token a |] in
  let n = node_intern nt ~kind:1 ~text_len:6 ~payload:0 cs in
  (* Read, modify and rebuild over one buffer, the idiom [Green.children_array]
     invites by handing back a copy that is "safe to mutate". *)
  cs.(1) <- Token b;
  let source (n : node) =
    Array.to_list n.nd_children
    |> List.map (function
      | Token t -> t.tk_text
      | Node _ -> "?")
    |> String.concat ""
  in
  Alcotest.(check string)
    "children survive a write to the caller's array"
    "aaaaaa"
    (source n);
  Alcotest.(check int) "text_len still matches the children" 6 n.nd_text_len;
  (* Still in the bucket its hash indexes, so its own shape finds it again in
     place of allocating a second record. *)
  let again = node_intern nt ~kind:1 ~text_len:6 ~payload:0 [| Token a; Token a |] in
  Helpers.same "original shape re-interns to the same record" n again
;;

let test_empty_children () =
  let nt = Siesta.Dedup.node_create () in
  let a = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:0 ~payload:0 [||] in
  let b = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:0 ~payload:0 [||] in
  Helpers.same "empty children dedupes" a b
;;

let test_deep_chain () =
  (* A 1000-deep chain, one child per node, so [node_hash] and interning meet
     deep nesting. *)
  let nt = Siesta.Dedup.node_create () in
  let depth = 1000 in
  let leaf = Siesta.Dedup.node_intern nt ~kind:0 ~text_len:0 ~payload:0 [||] in
  let rec build n cur =
    if n = 0
    then cur
    else (
      let next =
        Siesta.Dedup.node_intern
          nt
          ~kind:1
          ~text_len:0
          ~payload:0
          [| Siesta.Dedup.Node cur |]
      in
      build (n - 1) next)
  in
  let top = build depth leaf in
  Siesta.Dedup.(Alcotest.(check int) "deep chain: top node kind" 1 top.nd_kind)
;;

let test_wide_array () =
  (* One node with 5000 children, to stress [same_children]. *)
  let t = Siesta.Dedup.token_create () in
  let nt = Siesta.Dedup.node_create () in
  let n = 5000 in
  let children =
    Array.init n (fun i ->
      Siesta.Dedup.Token (Siesta.Dedup.token_intern t ~kind:0 ~text:(string_of_int i)))
  in
  let a = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:n ~payload:0 children in
  let b = Siesta.Dedup.node_intern nt ~kind:1 ~text_len:n ~payload:0 children in
  Helpers.same (Printf.sprintf "wide array: %d-child node dedupes" n) a b;
  Alcotest.(check Testable.dedup_node) "wide array: interned nodes equal" a b
;;

(* -- hash distribution sanity ---------------------------------------------- *)

(* A sanity check on the hash: the biggest bucket should stay within a small
   constant factor of the median one.

   The tokens are held until after the stats are read. Dropped, most of them get
   collected before the measurement and it reports the shape of a mostly-empty
   table: on OCaml 5.5 that came out as 1560 live of 10000, median bucket 0, and
   the check then means nothing. *)
let test_hash_distribution () =
  let t = Siesta.Dedup.token_create () in
  let n = 10_000 in
  let kept =
    Array.init n (fun i ->
      Siesta.Dedup.token_intern t ~kind:(i mod 100) ~text:(string_of_int i))
  in
  let _, entries, _, _, median, biggest = Siesta.Dedup.token_stats t in
  ignore (Sys.opaque_identity kept);
  Printf.printf
    "  hash distribution: entries=%d median=%d biggest=%d\n"
    entries
    median
    biggest;
  let median = max 1 median in
  Alcotest.(check bool)
    (Printf.sprintf "biggest_bucket (%d) ≤ 6 × median_bucket (%d)" biggest median)
    true
    (biggest <= 6 * median)
;;

(* -- childless node, payload entropy in the high bits ---------------------- *)

(* On a childless node [payload] is the last word [node_hash] mixes, and [mix]
   leaves its high bits out of the bucket index. Caller payloads that carry
   their entropy up there, a packed span or an id, then put every node in one
   bucket and interning goes quadratic. [final] is what stops that. *)
let test_childless_payload_high_bits () =
  let t = Siesta.Dedup.node_create () in
  let n = 10_000 in
  let keep =
    Array.init n (fun i ->
      Siesta.Dedup.node_intern t ~kind:5 ~text_len:0 ~payload:(i lsl 43) [||])
  in
  let _, entries, _, _, _, biggest = Siesta.Dedup.node_stats t in
  Printf.printf "  childless payload: entries=%d biggest=%d\n" entries biggest;
  Alcotest.(check int) "every payload interned distinctly" n entries;
  Alcotest.(check bool)
    (Printf.sprintf "biggest_bucket (%d) ≤ 64, not ~n (%d)" biggest n)
    true
    (biggest <= 64);
  (* And [final] must leave the hit path alone: re-interning a live key still
     returns the record already there. *)
  Alcotest.(check Testable.dedup_node)
    "re-intern hits the existing record"
    keep.(0)
    (Siesta.Dedup.node_intern t ~kind:5 ~text_len:0 ~payload:0 [||])
;;

let () =
  Alcotest.run
    "dedup"
    [ ( "tokens"
      , [ Alcotest.test_case "idempotence" `Quick test_token_idempotence
        ; Alcotest.test_case "distinctness" `Quick test_token_distinctness
        ; Alcotest.test_case "empty text" `Quick test_empty_token
        ] )
    ; ( "nodes"
      , [ Alcotest.test_case "idempotence" `Quick test_node_idempotence
        ; Alcotest.test_case "distinctness" `Quick test_node_distinctness
        ; Alcotest.test_case
            "intern copies children"
            `Quick
            test_node_intern_copies_children
        ; Alcotest.test_case "empty children" `Quick test_empty_children
        ] )
    ; ( "table"
      , [ Alcotest.test_case "tag monotonicity" `Quick test_tag_monotonicity
        ; Alcotest.test_case "collisions" `Quick test_collision
        ; Alcotest.test_case "resize" `Quick test_resize
        ; Alcotest.test_case "deep chain" `Quick test_deep_chain
        ; Alcotest.test_case "wide array" `Quick test_wide_array
        ; Alcotest.test_case "hash distribution" `Quick test_hash_distribution
        ; Alcotest.test_case
            "childless payload high bits"
            `Quick
            test_childless_payload_high_bits
        ] )
    ]
;;
