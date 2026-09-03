open Siesta

let mk_tok = Helpers.mk_tok
let mk_node = Helpers.mk_node

module K = struct
  let int_lit = 1
  let plus = 2
  let bin_expr = 10
end

(* `1+1` as a single BIN_EXPR of three tokens. *)
let build_1p1 cache =
  let b = Builder.create ~cache () in
  Builder.start_node b K.bin_expr;
  Builder.token b K.int_lit "1";
  Builder.token b K.plus "+";
  Builder.token b K.int_lit "1";
  Builder.finish_node b;
  Builder.finish b
;;

(* `(1+2)+(3+4)`; root BIN_EXPR over [BIN(1,+,2); +; BIN(3,+,4)]. *)
let build_arith4 cache =
  let b = Builder.create ~cache () in
  let pair l r =
    Builder.start_node b K.bin_expr;
    Builder.token b K.int_lit l;
    Builder.token b K.plus "+";
    Builder.token b K.int_lit r;
    Builder.finish_node b
  in
  Builder.start_node b K.bin_expr;
  pair "1" "2";
  Builder.token b K.plus "+";
  pair "3" "4";
  Builder.finish_node b;
  Builder.finish b
;;

(* -- identity: equal values are distinct, not shared ----------------------- *)

let test_plain_tokens_distinct () =
  let c = Cache.create_plain () in
  let a = mk_tok c 1 "foo" in
  let b = mk_tok c 1 "foo" in
  Helpers.distinct "Plain: structurally-equal tokens are not shared" a b;
  Alcotest.(check bool)
    "Plain: equal tokens get distinct tags"
    true
    (Green.Token.tag a <> Green.Token.tag b);
  (* fields are still what was asked for *)
  Alcotest.(check int) "kind preserved" 1 (Green.Token.kind a);
  Alcotest.(check string) "text preserved" "foo" (Green.Token.text a);
  (* Green.equal is tag equality: reflexive, but separates the two *)
  Alcotest.(check bool) "token equals itself" true (Green.Token.equal a a);
  Alcotest.(check bool) "distinct tokens compare unequal" false (Green.Token.equal a b)
;;

let test_plain_nodes_distinct () =
  let c = Cache.create_plain () in
  (* One shared token value, so the two children arrays are structurally
     identical and Plain still has to hand back two separate nodes. *)
  let one = mk_tok c 1 "1" in
  let a = mk_node c K.bin_expr [| Green.Token one |] in
  let b = mk_node c K.bin_expr [| Green.Token one |] in
  Helpers.distinct "Plain: structurally-equal nodes are not shared" a b;
  Alcotest.(check bool)
    "Plain: equal nodes get distinct tags"
    true
    (Green.tag a <> Green.tag b);
  Alcotest.(check bool) "node equals itself" true (Green.equal a a);
  Alcotest.(check bool) "distinct nodes compare unequal" false (Green.equal a b)
;;

(* Plain allocates its record directly rather than going through the weak
   tables, so it needs the same defensive copy: no bucket to strand an entry in,
   but [text_len] would still come to disagree with the children it was summed
   from, and [Syntax] derives every cursor offset from [text_len]. *)
let test_plain_mk_node_copies_children () =
  let c = Cache.create_plain () in
  let a = mk_tok c 1 "aaa" in
  let b = mk_tok c 1 "b" in
  let cs = [| Green.Token a; Green.Token a |] in
  let n = mk_node c K.bin_expr cs in
  cs.(1) <- Green.Token b;
  Alcotest.(check string)
    "Plain: children survive a write to the caller's array"
    "aaaaaa"
    (Green.to_source n);
  Alcotest.(check int)
    "Plain: text_len still matches to_source"
    (String.length (Green.to_source n))
    (Green.text_len n)
;;

(* Same shape, same Plain cache, two builds, two distinct roots. That is the
   defining difference from a Hashconsed cache. *)
let test_plain_no_cross_build_sharing () =
  let c = Cache.create_plain () in
  let r1 = build_1p1 c in
  let r2 = build_1p1 c in
  Helpers.distinct "Plain: same shape twice is not shared" r1 r2;
  Alcotest.(check bool) "distinct tags" true (Green.tag r1 <> Green.tag r2);
  Alcotest.(check string)
    "but structurally equivalent"
    (Green.to_source r1)
    (Green.to_source r2)
;;

let test_plain_tag_disjoint_from_hashconsed () =
  (* Both modes draw from the one global counter, so handles from a Plain cache
     and a Hashconsed cache never collide. *)
  let plain = Cache.create_plain () in
  let hashed = Cache.create () in
  let a = mk_tok plain 1 "z" in
  let b = mk_tok hashed 1 "z" in
  Alcotest.(check bool)
    "Plain and Hashconsed tags are disjoint"
    true
    (Green.Token.tag a <> Green.Token.tag b)
;;

(* -- structure is still correct -------------------------------------------- *)

let test_plain_roundtrip () =
  let plain = build_arith4 (Cache.create_plain ()) in
  let hashed = build_arith4 (Cache.create ()) in
  Alcotest.(check string) "Plain to_source" "1+2+3+4" (Green.to_source plain);
  Alcotest.(check int) "Plain text_len" 7 (Green.text_len plain);
  (* pp prints kinds and token text, never tags, so the same shape gives the
     same string whichever mode built it. *)
  Alcotest.(check string)
    "Plain pp == Hashconsed pp"
    (Format.asprintf "%a" Green.pp hashed)
    (Format.asprintf "%a" Green.pp plain)
;;

let test_plain_stats_zero () =
  let c = Cache.create_plain () in
  let _ = build_arith4 c in
  (* Plain keeps no table, so stats are all zero even after building. *)
  Alcotest.(check int) "token entries zero" 0 Cache.((token_stats c).entries);
  Alcotest.(check int) "token table_length zero" 0 Cache.((token_stats c).table_length);
  Alcotest.(check int) "node entries zero" 0 Cache.((node_stats c).entries);
  Alcotest.(check int) "node table_length zero" 0 Cache.((node_stats c).table_length)
;;

let test_plain_clear_noop () =
  (* [clear] has nothing to drop in Plain mode, so it has to leave the cache
     usable and the tags advancing from the global counter. *)
  let c = Cache.create_plain () in
  let a = mk_tok c 1 "x" in
  Cache.clear c;
  let b = mk_tok c 1 "x" in
  Helpers.distinct "still fresh after clear" a b;
  Alcotest.(check bool)
    "post-clear tag advances"
    true
    (Green.Token.tag b > Green.Token.tag a);
  let root = build_1p1 c in
  Alcotest.(check string) "cache usable after clear" "1+1" (Green.to_source root)
;;

(* -- red layer over a Plain-built tree ------------------------------------- *)

let test_plain_syntax_replace () =
  let cache = Cache.create_plain () in
  let g = build_arith4 cache in
  let old_rhs =
    let cs = Green.children_array g in
    if Array.length cs <> 3 then Alcotest.fail "expected node/token/node";
    match cs.(2) with
    | Green.Node rhs -> rhs
    | Green.Token _ -> Alcotest.fail "expected node/token/node"
  in
  let r = Syntax.of_root g in
  let lhs =
    match (Syntax.children_array r).(0) with
    | Syntax.Node c -> c
    | Syntax.Token _ -> Alcotest.fail "expected node child"
  in
  let repl = build_1p1 cache in
  let { Syntax.root = new_root; self = new_lhs } = Syntax.replace cache lhs repl in
  Alcotest.(check string) "replace to_source" "1+1+3+4" (Syntax.to_source new_root);
  (* The off-spine RHS is carried over by reference, so it stays physically
     shared even under a Plain cache. *)
  let new_rhs =
    let cs = Green.children_array (Syntax.green new_root) in
    if Array.length cs <> 3 then Alcotest.fail "expected node/token/node";
    match cs.(2) with
    | Green.Node rhs -> rhs
    | Green.Token _ -> Alcotest.fail "expected node/token/node"
  in
  Helpers.same "Plain: off-spine RHS still shared after replace" old_rhs new_rhs;
  Helpers.same "self carries replacement green" (Syntax.green new_lhs) repl
;;

let test_plain_replace_reallocates_spine () =
  (* Under Plain, replacing a non-root node with its own green still allocates
     a fresh spine above it, so the new root reads the same but carries a new
     tag. Hashconsed would hand back the original. *)
  let cache = Cache.create_plain () in
  let g = build_arith4 cache in
  let r = Syntax.of_root g in
  let lhs =
    match (Syntax.children_array r).(0) with
    | Syntax.Node c -> c
    | Syntax.Token _ -> Alcotest.fail "expected node child"
  in
  let { Syntax.root = new_root; _ } = Syntax.replace cache lhs (Syntax.green lhs) in
  let new_root_g = Syntax.green new_root in
  Alcotest.(check string)
    "source unchanged"
    (Green.to_source g)
    (Green.to_source new_root_g);
  Alcotest.(check bool)
    "Plain: rebuilt root is a fresh value (no spine dedup)"
    false
    (Green.equal g new_root_g);
  Helpers.distinct "Plain: rebuilt root not physically shared" g new_root_g
;;

let () =
  Alcotest.run
    "plain"
    [ ( "plain cache mode"
      , [ Alcotest.test_case "tokens distinct" `Quick test_plain_tokens_distinct
        ; Alcotest.test_case "nodes distinct" `Quick test_plain_nodes_distinct
        ; Alcotest.test_case
            "mk_node copies children"
            `Quick
            test_plain_mk_node_copies_children
        ; Alcotest.test_case
            "no cross-build sharing"
            `Quick
            test_plain_no_cross_build_sharing
        ; Alcotest.test_case
            "tags disjoint from hashconsed"
            `Quick
            test_plain_tag_disjoint_from_hashconsed
        ; Alcotest.test_case "roundtrip == hashconsed" `Quick test_plain_roundtrip
        ; Alcotest.test_case "stats zero" `Quick test_plain_stats_zero
        ; Alcotest.test_case "clear is a no-op" `Quick test_plain_clear_noop
        ; Alcotest.test_case
            "syntax replace + off-spine share"
            `Quick
            test_plain_syntax_replace
        ; Alcotest.test_case
            "replace reallocates spine"
            `Quick
            test_plain_replace_reallocates_spine
        ] )
    ]
;;
