open Siesta

let mk_tok = Helpers.mk_tok
let mk_node = Helpers.mk_node

let arity what n cs =
  if Array.length cs <> n
  then Alcotest.failf "%s: expected %d children, got %d" what n (Array.length cs)
;;

let g_node what cs i =
  match cs.(i) with
  | Green.Node n -> n
  | Green.Token t ->
    Alcotest.failf "%s: child %d is token %S, want a node" what i (Green.Token.text t)
;;

let g_token what cs i =
  match cs.(i) with
  | Green.Token t -> t
  | Green.Node n ->
    Alcotest.failf "%s: child %d is node K%d, want a token" what i (Green.kind n)
;;

let s_token what cs i =
  match cs.(i) with
  | Syntax.Token t -> t
  | Syntax.Node n ->
    Alcotest.failf "%s: child %d is node K%d, want a token" what i (Syntax.kind n)
;;

(* -- tag identity ---------------------------------------------------------- *)

let test_token_tag_identity () =
  let c = Cache.create () in
  let a = mk_tok c 1 "foo" in
  let b = mk_tok c 1 "foo" in
  Helpers.same "structurally-equal tokens are physically shared" a b;
  Alcotest.(check int) "shared tag" (Green.Token.tag a) (Green.Token.tag b)
;;

let test_token_distinct_text () =
  let c = Cache.create () in
  let a = mk_tok c 1 "foo" in
  let b = mk_tok c 1 "bar" in
  Alcotest.(check bool)
    "different text → distinct tags"
    true
    (Green.Token.tag a <> Green.Token.tag b)
;;

let test_token_distinct_kind () =
  let c = Cache.create () in
  let a = mk_tok c 1 "x" in
  let b = mk_tok c 2 "x" in
  Alcotest.(check bool)
    "different kind → distinct tags"
    true
    (Green.Token.tag a <> Green.Token.tag b)
;;

let test_node_tag_identity () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let n1 = mk_node c 10 [| Green.Token one |] in
  let n2 = mk_node c 10 [| Green.Token one |] in
  Helpers.same "structurally-equal nodes are physically shared" n1 n2;
  Alcotest.(check int) "shared tag" (Green.tag n1) (Green.tag n2)
;;

let test_node_distinct_kind () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let a = mk_node c 10 [| Green.Token one |] in
  let b = mk_node c 11 [| Green.Token one |] in
  Alcotest.(check bool) "different kind → distinct tags" true (Green.tag a <> Green.tag b)
;;

let test_node_distinct_arity () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let a = mk_node c 10 [| Green.Token one |] in
  let b = mk_node c 10 [| Green.Token one; Green.Token one |] in
  Alcotest.(check bool) "different arity → distinct tags" true (Green.tag a <> Green.tag b)
;;

(* Payload takes part in identity, so two otherwise-identical nodes with
   different payloads are separate cache entries. *)
let test_node_payload_distinct () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let a = Green.mk_node c ~kind:10 ~payload:1 ~children:[| Green.Token one |] () in
  let b = Green.mk_node c ~kind:10 ~payload:2 ~children:[| Green.Token one |] () in
  Alcotest.(check bool)
    "distinct payloads → distinct tags"
    true
    (Green.tag a <> Green.tag b);
  Alcotest.(check int) "payload a" 1 (Green.payload a);
  Alcotest.(check int) "payload b" 2 (Green.payload b)
;;

let test_node_payload_zero_shared () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let a = Green.mk_node c ~kind:10 ~children:[| Green.Token one |] () in
  let b = Green.mk_node c ~kind:10 ~payload:0 ~children:[| Green.Token one |] () in
  Helpers.same "default payload (0) preserves hash-consing" a b;
  Alcotest.(check int) "payload is zero" 0 (Green.payload a)
;;

(* -- subtree sharing ------------------------------------------------------- *)

(* `1 + 1`, where the two `1` tokens share. *)
let test_one_plus_one_token_shared () =
  let c = Cache.create () in
  let lhs = mk_tok c 1 "1" in
  let op = mk_tok c 2 "+" in
  let rhs = mk_tok c 1 "1" in
  Helpers.same "the two `1` tokens share" lhs rhs;
  let (_ : Green.node) =
    mk_node c 10 [| Green.Token lhs; Green.Token op; Green.Token rhs |]
  in
  ()
;;

(* `(1+1) + (1+1)`, where the inner BIN_EXPR is shared. *)
let test_inner_bin_shared () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let plus = mk_tok c 2 "+" in
  let inner_a = mk_node c 10 [| Green.Token one; Green.Token plus; Green.Token one |] in
  let inner_b = mk_node c 10 [| Green.Token one; Green.Token plus; Green.Token one |] in
  Alcotest.check Testable.green "inner BIN_EXPR shared" inner_a inner_b;
  Alcotest.(check int) "inner tag" (Green.tag inner_a) (Green.tag inner_b);
  let outer =
    mk_node c 10 [| Green.Node inner_a; Green.Token plus; Green.Node inner_b |]
  in
  (* And the outer node points at that one shared inner. *)
  let what = "outer shape" in
  let cs = Green.children_array outer in
  arity what 3 cs;
  Alcotest.(check int)
    "outer points to shared inner"
    (Green.tag (g_node what cs 0))
    (Green.tag (g_node what cs 2))
;;

(* Same children under a different parent kind: distinct parents, one shared
   child subtree. *)
let test_off_spine_sharing () =
  let c = Cache.create () in
  let one = mk_tok c 1 "1" in
  let plus = mk_tok c 2 "+" in
  let inner = mk_node c 10 [| Green.Token one; Green.Token plus; Green.Token one |] in
  let parent_a = mk_node c 20 [| Green.Node inner |] in
  let parent_b = mk_node c 21 [| Green.Node inner |] in
  Alcotest.(check bool)
    "distinct parent kinds → distinct tags"
    true
    (Green.tag parent_a <> Green.tag parent_b);
  let what = "parent shape" in
  let csa = Green.children_array parent_a
  and csb = Green.children_array parent_b in
  arity what 1 csa;
  arity what 1 csb;
  Alcotest.(check int)
    "distinct parents share their child subtree"
    (Green.tag (g_node what csa 0))
    (Green.tag (g_node what csb 0))
;;

(* -- weak-table sanity ----------------------------------------------------- *)

let test_weak_collects_unreferenced () =
  let c = Cache.create () in
  for i = 0 to 4_999 do
    let _ = mk_tok c (i land 0xff) (Printf.sprintf "tok_%d" i) in
    ()
  done;
  let before = Cache.((token_stats c).entries) in
  Gc.full_major ();
  Gc.full_major ();
  let after = Cache.((token_stats c).entries) in
  (* The live count has to fall sharply once the strong refs are gone. *)
  Alcotest.(check bool)
    (Printf.sprintf
       "weak table releases unreferenced entries (before=%d after=%d)"
       before
       after)
    true
    (after < before / 2)
;;

let test_cache_clear () =
  let c = Cache.create () in
  let _kept = Array.init 100 (fun i -> mk_tok c (i mod 7) (Printf.sprintf "k_%d" i)) in
  let before = Cache.((token_stats c).entries) in
  Alcotest.(check bool) "cache populated before clear" true (before > 0);
  Cache.clear c;
  Alcotest.(check int) "cache empty after clear" 0 Cache.((token_stats c).entries);
  (* The entry from before is gone, so an equal value re-interned after the
     clear gets a new tag. *)
  let t1 = mk_tok c 1 "x" in
  let tag1 = Green.Token.tag t1 in
  Cache.clear c;
  let t2 = mk_tok c 1 "x" in
  Alcotest.(check bool)
    "re-insert after clear gets a fresh tag"
    true
    (Green.Token.tag t2 <> tag1)
;;

(* Held entries survive GC and remain physically equal across hashcons. *)
let test_weak_held_entries_survive () =
  let c = Cache.create () in
  let kept =
    Array.init 1024 (fun i -> mk_tok c (i mod 13) (Printf.sprintf "kept_%d" i))
  in
  Gc.full_major ();
  Array.iteri
    (fun i v ->
       let v' = mk_tok c (i mod 13) (Printf.sprintf "kept_%d" i) in
       Helpers.same "held entry survives GC and round-trips" v v';
       Alcotest.(check int)
         "held entry tag stable"
         (Green.Token.tag v)
         (Green.Token.tag v'))
    kept
;;

(* -- hash distribution ----------------------------------------------------- *)

(* Insert N distinct tokens varying in both kind and text, then check no bucket
   is grossly oversized against the median. A sanity check on the hash. *)
let test_hash_distribution_tokens () =
  let c = Cache.create () in
  let n = 4096 in
  let kept = Array.init n (fun i -> mk_tok c (i mod 17) (Printf.sprintf "id_%d" i)) in
  let s = Cache.token_stats c in
  ignore kept;
  Cache.(
    Alcotest.(check int) "all entries present" n s.entries;
    Alcotest.(check bool) "table grew" true (s.table_length > 0);
    Alcotest.(check bool) "occupancy sane" true (s.sum_bucket_lengths >= s.entries);
    (* A generous bound on the worst bucket. *)
    Alcotest.(check bool)
      (Printf.sprintf
         "worst bucket not catastrophic (median=%d max=%d)"
         s.median_bucket
         s.biggest_bucket)
      true
      (s.biggest_bucket <= max 16 (8 * max 1 s.median_bucket)))
;;

(* Same idea for nodes, where the children are already tagged ints. *)
let test_hash_distribution_nodes () =
  let c = Cache.create () in
  let toks = Array.init 64 (fun i -> mk_tok c (i mod 5) (string_of_int i)) in
  let n = 2048 in
  let kept =
    Array.init n (fun i ->
      let a = toks.(i mod 64) in
      let b = toks.(i / 64 mod 64) in
      mk_node c (i mod 11) [| Green.Token a; Green.Token b |])
  in
  let s = Cache.node_stats c in
  ignore kept;
  Cache.(
    Alcotest.(check int) "all entries present" n s.entries;
    Alcotest.(check bool)
      (Printf.sprintf
         "worst bucket not catastrophic (median=%d max=%d)"
         s.median_bucket
         s.biggest_bucket)
      true
      (s.biggest_bucket <= max 16 (8 * max 1 s.median_bucket)))
;;

(* -- Builder + public Green surface ---------------------------------------- *)

(* A tiny kind enum for the tests. A real consumer defines its own. *)
module K = struct
  let int_lit = 1 (* token: digits *)
  let plus = 2 (* token: + *)
  let bin_expr = 10
  let root = 20
  let chain = 30
  let leaf = 31
end

(* Build `1 + 1` and return the root. *)
let build_one_plus_one ?cache () =
  let b = Builder.create ?cache () in
  Builder.start_node b K.bin_expr;
  Builder.token b K.int_lit "1";
  Builder.token b K.plus "+";
  Builder.token b K.int_lit "1";
  Builder.finish_node b;
  Builder.finish b
;;

let test_builder_one_plus_one () =
  let root = build_one_plus_one () in
  Alcotest.(check int) "root kind" K.bin_expr (Green.kind root);
  let cs = Green.children_array root in
  Alcotest.(check int) "child count" 3 (Array.length cs);
  let what = "three token children" in
  let lhs, op, rhs = g_token what cs 0, g_token what cs 1, g_token what cs 2 in
  Alcotest.(check int) "lhs kind" K.int_lit (Green.Token.kind lhs);
  Alcotest.(check int) "op kind" K.plus (Green.Token.kind op);
  Alcotest.(check int) "rhs kind" K.int_lit (Green.Token.kind rhs);
  Alcotest.check Testable.green_token "lhs and rhs are the shared `1`" lhs rhs;
  (* One tag, two positions. *)
  Alcotest.(check int) "shared tag" (Green.Token.tag lhs) (Green.Token.tag rhs);
  Alcotest.(check bool) "lhs ≠ op despite both tokens" false (Green.Token.equal lhs op)
;;

let test_builder_text_len () =
  let root = build_one_plus_one () in
  Alcotest.(check int) "text_len" 3 (Green.text_len root);
  Alcotest.(check string) "to_source" "1+1" (Green.to_source root)
;;

(* Build `(1+1) + (1+1)` and verify the inner BIN_EXPR is shared. *)
let test_builder_inner_bin_shared () =
  let b = Builder.create () in
  let bin () =
    Builder.start_node b K.bin_expr;
    Builder.token b K.int_lit "1";
    Builder.token b K.plus "+";
    Builder.token b K.int_lit "1";
    Builder.finish_node b
  in
  Builder.start_node b K.bin_expr;
  bin ();
  Builder.token b K.plus "+";
  bin ();
  Builder.finish_node b;
  let root = Builder.finish b in
  let cs = Green.children_array root in
  let what = "node/token/node" in
  arity what 3 cs;
  let inner_a, inner_b = g_node what cs 0, g_node what cs 2 in
  Alcotest.check Testable.green "inner BIN_EXPR shared" inner_a inner_b;
  Alcotest.(check int) "inner tag" (Green.tag inner_a) (Green.tag inner_b);
  Alcotest.(check int) "text_len" 7 (Green.text_len root);
  Alcotest.(check string) "to_source" "1+1+1+1" (Green.to_source root)
;;

(* Hash-consing crosses builder instances when they share a cache. *)
let test_cross_builder_sharing () =
  let cache = Cache.create () in
  let r1 = build_one_plus_one ~cache () in
  let r2 = build_one_plus_one ~cache () in
  Alcotest.check Testable.green "shared cache → identical roots" r1 r2;
  Alcotest.(check int) "identical tag" (Green.tag r1) (Green.tag r2)
;;

(* Caches are isolated, so two builds with disjoint caches come out
   structurally equal but tag-distinct. *)
let test_disjoint_caches () =
  let r1 = build_one_plus_one () in
  let r2 = build_one_plus_one () in
  Alcotest.(check string) "same source" (Green.to_source r1) (Green.to_source r2);
  Alcotest.(check bool)
    "disjoint caches → distinct tags"
    true
    (Green.tag r1 <> Green.tag r2)
;;

(* Build `1 + 2` with a checkpoint, verify identical to direct nesting. *)
let test_builder_checkpoint () =
  let direct =
    let b = Builder.create () in
    Builder.start_node b K.root;
    Builder.start_node b K.bin_expr;
    Builder.token b K.int_lit "1";
    Builder.token b K.plus "+";
    Builder.token b K.int_lit "2";
    Builder.finish_node b;
    Builder.finish_node b;
    Builder.finish b
  in
  let via_checkpoint =
    let b = Builder.create ~cache:(Cache.create ()) (* a fresh cache *) () in
    Builder.start_node b K.root;
    let cp = Builder.checkpoint b in
    Builder.token b K.int_lit "1";
    Builder.start_node_at b cp K.bin_expr;
    Builder.token b K.plus "+";
    Builder.token b K.int_lit "2";
    Builder.finish_node b;
    Builder.finish_node b;
    Builder.finish b
  in
  (* Different caches, so the tags differ and the comparison has to go through
     source and pp instead. *)
  Alcotest.(check string)
    "checkpoint == direct (source)"
    (Green.to_source direct)
    (Green.to_source via_checkpoint);
  Alcotest.(check string)
    "checkpoint == direct (pp)"
    (Format.asprintf "%a" Green.pp direct)
    (Format.asprintf "%a" Green.pp via_checkpoint);
  (* And, with a shared cache, identical tags. *)
  let cache = Cache.create () in
  let direct_c =
    let b = Builder.create ~cache () in
    Builder.start_node b K.root;
    Builder.start_node b K.bin_expr;
    Builder.token b K.int_lit "1";
    Builder.token b K.plus "+";
    Builder.token b K.int_lit "2";
    Builder.finish_node b;
    Builder.finish_node b;
    Builder.finish b
  in
  let cp_c =
    let b = Builder.create ~cache () in
    Builder.start_node b K.root;
    let cp = Builder.checkpoint b in
    Builder.token b K.int_lit "1";
    Builder.start_node_at b cp K.bin_expr;
    Builder.token b K.plus "+";
    Builder.token b K.int_lit "2";
    Builder.finish_node b;
    Builder.finish_node b;
    Builder.finish b
  in
  Alcotest.check Testable.green "shared cache → structurally equal" direct_c cp_c;
  Alcotest.(check int) "shared cache → same tag" (Green.tag direct_c) (Green.tag cp_c)
;;

(* A checkpoint belongs to the frame it was taken in. Using one after that
   frame has closed, or once a deeper frame sits on top, has to fail loudly
   instead of quietly wrapping the wrong children. *)
let test_checkpoint_rejected_after_frame_closes () =
  let b = Builder.create () in
  Builder.start_node b K.root;
  Builder.start_node b K.bin_expr;
  let cp = Builder.checkpoint b in
  (* taken inside bin_expr *)
  Builder.token b K.int_lit "1";
  Builder.finish_node b;
  (* closes bin_expr; root is top *)
  try
    Builder.start_node_at b cp K.bin_expr;
    Alcotest.fail "checkpoint from a closed frame must be rejected"
  with
  | Failure _ -> ()
;;

let test_checkpoint_rejected_when_buried () =
  let b = Builder.create () in
  Builder.start_node b K.root;
  let cp = Builder.checkpoint b in
  (* taken in root *)
  Builder.start_node b K.bin_expr;
  (* root is no longer top *)
  try
    Builder.start_node_at b cp K.bin_expr;
    Alcotest.fail "checkpoint buried under a deeper frame must be rejected"
  with
  | Failure _ -> ()
;;

(* Two checkpoints from one frame, the earlier reused first.

   Both carry the frame's gen, so the gen guard lets each through. Reusing
   [cp1] wraps everything from position 0 and shrinks the buffer to a single
   entry, which leaves [cp2] pointing past the end. That is what the position
   guard in [start_node_at] is for. Without it the wrap silently produced a
   childless node and stranded the child it was meant to swallow, with source
   and text_len both still self-consistent. *)
let test_checkpoint_rejected_when_position_stale () =
  let b = Builder.create () in
  Builder.start_node b K.root;
  let cp1 = Builder.checkpoint b in
  Builder.token b K.int_lit "a";
  Builder.token b K.int_lit "b";
  let cp2 = Builder.checkpoint b in
  Builder.token b K.int_lit "c";
  (* wraps a, b, c, so the buffer is back to a single entry *)
  Builder.start_node_at b cp1 K.bin_expr;
  Builder.finish_node b;
  try
    Builder.start_node_at b cp2 K.bin_expr;
    Alcotest.fail "checkpoint left past the end by an earlier reuse must be rejected"
  with
  | Failure msg ->
    Alcotest.(check bool)
      (Printf.sprintf "rejected as stale, not by the gen guard (got %S)" msg)
      true
      (String.length msg >= 5
       &&
       let n = String.length msg in
       let rec has i = i + 5 <= n && (String.sub msg i 5 = "stale" || has (i + 1)) in
       has 0)
;;

(* The other order still works. A checkpoint taken later and used before any
   earlier one is reused is untouched, so the guard has to let it through.
   Paired with the test above, so "reject everything" cannot pass both. *)
let test_checkpoint_inner_first_still_wraps () =
  let cache = Cache.create () in
  let b = Builder.create ~cache () in
  Builder.start_node b K.root;
  let cp1 = Builder.checkpoint b in
  Builder.token b K.int_lit "a";
  let cp2 = Builder.checkpoint b in
  Builder.token b K.int_lit "b";
  Builder.start_node_at b cp2 K.bin_expr;
  Builder.finish_node b;
  Builder.start_node_at b cp1 K.chain;
  Builder.finish_node b;
  Builder.finish_node b;
  let root = Builder.finish b in
  Alcotest.(check string) "source survives both wraps" "ab" (Green.to_source root);
  Alcotest.(check string)
    "inner-first nesting"
    "(K20\n  (K30\n    (K1 \"a\")\n    (K10\n      (K1 \"b\"))))"
    (Format.asprintf "%a" Green.pp root)
;;

(* The left-associative parser idiom: capture once, reuse each time round. The
   same [cp] still points at its original write offset after every
   [start_node_at]/[finish_node] pair, so the next wrap swallows the node the
   last one produced. Builds 1+2+3 as BIN(BIN(1,+,2),+,3). *)
let test_checkpoint_reuse_left_assoc () =
  let b = Builder.create () in
  Builder.start_node b K.root;
  let cp = Builder.checkpoint b in
  Builder.token b K.int_lit "1";
  Builder.start_node_at b cp K.bin_expr;
  Builder.token b K.plus "+";
  Builder.token b K.int_lit "2";
  Builder.finish_node b;
  Builder.start_node_at b cp K.bin_expr;
  Builder.token b K.plus "+";
  Builder.token b K.int_lit "3";
  Builder.finish_node b;
  Builder.finish_node b;
  let root = Builder.finish b in
  Alcotest.(check string) "left-assoc source" "1+2+3" (Green.to_source root);
  let outer =
    let what = "single node child" in
    let cs = Green.children_array root in
    arity what 1 cs;
    g_node what cs 0
  in
  Alcotest.(check int) "outer kind" K.bin_expr (Green.kind outer);
  let inner =
    let what = "node/token/token" in
    let cs = Green.children_array outer in
    arity what 3 cs;
    g_node what cs 0
  in
  Alcotest.(check int) "inner kind" K.bin_expr (Green.kind inner);
  Alcotest.(check string) "inner source" "1+2" (Green.to_source inner)
;;

(* Deep tree: 10000 nested CHAIN nodes around a single LEAF. *)
let test_deep_tree () =
  let depth = 10_000 in
  let b = Builder.create () in
  for _ = 1 to depth do
    Builder.start_node b K.chain
  done;
  Builder.token b K.leaf "x";
  for _ = 1 to depth do
    Builder.finish_node b
  done;
  let root = Builder.finish b in
  Alcotest.(check int) "text_len" 1 (Green.text_len root);
  Alcotest.(check string) "to_source" "x" (Green.to_source root);
  (* Walk back down and count the levels. *)
  let n = ref root in
  let count = ref 0 in
  let rec descend () =
    let cs = Green.children_array !n in
    if Array.length cs = 1
    then (
      match cs.(0) with
      | Green.Node child ->
        n := child;
        incr count;
        descend ()
      | Green.Token _ -> ())
    else ()
  in
  descend ();
  (* The innermost CHAIN's child is a token, which stops the walk without
     counting, so the total comes to [depth - 1]. *)
  Alcotest.(check int) "walked full depth" (depth - 1) !count
;;

let test_pp () =
  let root = build_one_plus_one () in
  let s = Format.asprintf "%a" Green.pp root in
  let expected = "(K10\n  (K1 \"1\")\n  (K2 \"+\")\n  (K1 \"1\"))" in
  Alcotest.(check string) "pp tree format" expected s
;;

(* Builder error paths. "No node started" and "tree already finished" both
   leave the stack empty, but they are different mistakes and the messages have
   to tell them apart. *)
let test_builder_errors () =
  let contains s sub =
    let ls = String.length s
    and lsub = String.length sub in
    let rec loop i =
      if i + lsub > ls
      then false
      else if String.sub s i lsub = sub
      then true
      else loop (i + 1)
    in
    loop 0
  in
  let expect_msg substring f =
    try
      f ();
      Alcotest.failf "expected Failure containing %S" substring
    with
    | Failure msg ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" substring)
        true
        (contains msg substring)
  in
  let b = Builder.create () in
  expect_msg "no node started" (fun () -> Builder.token b K.int_lit "1");
  expect_msg "no node started" (fun () -> Builder.finish_node b);
  (try
     let _ = Builder.checkpoint b in
     Alcotest.fail "checkpoint before any node must fail"
   with
   | Failure _ -> ());
  Builder.start_node b K.root;
  (try
     let _ = Builder.finish b in
     Alcotest.fail "finish with an open node must fail"
   with
   | Failure _ -> ());
  Builder.token b K.int_lit "1";
  Builder.finish_node b;
  let _ = Builder.finish b in
  (* Finished now, so no more starts, tokens or finish_nodes. *)
  (try
     Builder.start_node b K.root;
     Alcotest.fail "start_node after finish must fail"
   with
   | Failure _ -> ());
  expect_msg "already finished" (fun () -> Builder.token b K.int_lit "x");
  expect_msg "already finished" (fun () -> Builder.finish_node b)
;;

(* -- Syntax (red) layer + replace ------------------------------------------ *)

(* `(1+2)+(3+4)` for spine/off-spine sharing tests. *)
let build_arith4 ?cache () =
  let b = Builder.create ?cache () in
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

let test_syntax_to_source () =
  let g = build_arith4 () in
  let r = Syntax.of_root g in
  Alcotest.(check string) "Syntax.to_source" "1+2+3+4" (Syntax.to_source r);
  Alcotest.(check int) "root kind" K.bin_expr (Syntax.kind r)
;;

let test_text_range () =
  let g = build_one_plus_one () in
  let r = Syntax.of_root g in
  Alcotest.(check (pair int int)) "root text_range" (0, 3) (Syntax.text_range r);
  let cs = Syntax.children_array r in
  Alcotest.(check int) "child count" 3 (Array.length cs);
  let what = "three token children" in
  Alcotest.(check (pair int int))
    "lhs range"
    (0, 1)
    (Syntax.Token.text_range (s_token what cs 0));
  Alcotest.(check (pair int int))
    "op range"
    (1, 2)
    (Syntax.Token.text_range (s_token what cs 1));
  Alcotest.(check (pair int int))
    "rhs range"
    (2, 3)
    (Syntax.Token.text_range (s_token what cs 2))
;;

let test_parent_navigation () =
  let g = build_arith4 () in
  let r = Syntax.of_root g in
  let cs = Syntax.children_array r in
  match cs.(0) with
  | Syntax.Node c ->
    (match Syntax.parent c with
     | Some p -> Helpers.same "child's parent is the root record" p r
     | None -> Alcotest.fail "expected a parent");
    let cs' = Syntax.children_array c in
    (match cs'.(2) with
     | Syntax.Token tok ->
       Helpers.same "token's parent is the same record" (Syntax.Token.parent tok) c;
       Alcotest.(check string) "token text" "2" (Syntax.Token.text tok);
       Alcotest.(check int) "token kind" K.int_lit (Syntax.Token.kind tok)
     | Syntax.Node _ -> Alcotest.fail "expected token child")
  | Syntax.Token _ -> Alcotest.fail "expected node child"
;;

let test_children_memoization () =
  let g = build_one_plus_one () in
  let r = Syntax.of_root g in
  let a = Syntax.children_array r in
  let b = Syntax.children_array r in
  Helpers.same "Syntax.children memoizes the cursor array" a b
;;

let test_equal_vs_same_tree () =
  let g = build_one_plus_one () in
  let r1 = Syntax.of_root g in
  let r2 = Syntax.of_root g in
  Alcotest.(check bool) "equal compares structurally" true (Syntax.equal r1 r2);
  Alcotest.(check bool) "same_tree compares root identity" false (Syntax.same_tree r1 r2);
  let cs1 = Syntax.children_array r1 in
  let cs2 = Syntax.children_array r2 in
  match cs1.(1), cs2.(1) with
  | Syntax.Token t1, Syntax.Token t2 ->
    (* Same offset and same green tag, so equal. *)
    Alcotest.(check bool) "tokens equal (offset + tag)" true (Syntax.Token.equal t1 t2);
    (* But the parent chains lead to two different roots. *)
    Alcotest.(check bool)
      "parents lead to different roots"
      false
      (Syntax.same_tree (Syntax.Token.parent t1) (Syntax.Token.parent t2))
  | Syntax.Node _, _ | _, Syntax.Node _ -> Alcotest.fail "expected token children"
;;

let test_replace_off_spine_share () =
  let cache = Cache.create () in
  let old_root_g = build_arith4 ~cache () in
  let old_rhs_g =
    let what = "node/token/node" in
    let cs = Green.children_array old_root_g in
    arity what 3 cs;
    g_node what cs 2
  in
  let r = Syntax.of_root old_root_g in
  let lhs_cursor =
    match (Syntax.children_array r).(0) with
    | Syntax.Node c -> c
    | Syntax.Token _ -> Alcotest.fail "expected node child"
  in
  (* Replacement: BIN_EXPR(5,+,6). *)
  let b2 = Builder.create ~cache () in
  Builder.start_node b2 K.bin_expr;
  Builder.token b2 K.int_lit "5";
  Builder.token b2 K.plus "+";
  Builder.token b2 K.int_lit "6";
  Builder.finish_node b2;
  let replacement_g = Builder.finish b2 in
  let { Syntax.root = new_root; self = new_lhs } =
    Syntax.replace cache lhs_cursor replacement_g
  in
  let new_root_g = Syntax.green new_root in
  (* The root has changed, so it carries a different green tag. *)
  Alcotest.(check bool) "root changed" false (Green.equal old_root_g new_root_g);
  let new_rhs_g =
    let what = "node/token/node" in
    let cs = Green.children_array new_root_g in
    arity what 3 cs;
    g_node what cs 2
  in
  Alcotest.check Testable.green "off-spine RHS structurally equal" old_rhs_g new_rhs_g;
  Helpers.same "off-spine RHS physically shared with old tree" old_rhs_g new_rhs_g;
  (* And self sits at the original target position, carrying the replacement. *)
  Helpers.same "new self carries replacement green" (Syntax.green new_lhs) replacement_g;
  Alcotest.(check int) "self index in parent" 0 (Syntax.index_in_parent new_lhs);
  Alcotest.(check string) "to_source" "5+6+3+4" (Syntax.to_source new_root)
;;

let test_replace_at_root () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  (* Replace the root with a single-token tree. *)
  let b = Builder.create ~cache () in
  Builder.start_node b K.bin_expr;
  Builder.token b K.int_lit "9";
  Builder.finish_node b;
  let replacement = Builder.finish b in
  let { Syntax.root = new_root; self = new_self } = Syntax.replace cache r replacement in
  Helpers.same
    "new_root carries the replacement green"
    (Syntax.green new_root)
    replacement;
  Helpers.same "root is its own self" new_root new_self;
  Alcotest.(check string) "to_source" "9" (Syntax.to_source new_root)
;;

let test_replace_idempotent_under_same_green () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  (* Replacing a node with a green identical to its current value has to
     hash-cons straight back to the tree that went in. *)
  let { Syntax.root = new_root; self = _ } = Syntax.replace cache r g in
  Helpers.same
    "replace with structurally-equal green deduplicates to original"
    (Syntax.green new_root)
    g
;;

let test_splice_insert () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  (* Append `+1` to the root. *)
  let plus_t = Green.mk_token cache ~kind:K.plus ~text:"+" in
  let one_t = Green.mk_token cache ~kind:K.int_lit ~text:"1" in
  let { Syntax.root = new_root; self = new_self } =
    Syntax.splice_children
      cache
      r
      ~at:3
      ~remove:0
      [ Green.Token plus_t; Green.Token one_t ]
  in
  Alcotest.(check string) "to_source" "1+1+1" (Syntax.to_source new_root);
  Alcotest.(check int) "text_len" 5 (Green.text_len (Syntax.green new_root));
  Helpers.same "target is the root" new_root new_self;
  Alcotest.(check int) "child count" 5 (Array.length (Syntax.children_array new_root))
;;

let test_splice_remove () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  let { Syntax.root = new_root; self = _ } =
    Syntax.splice_children cache r ~at:1 ~remove:2 []
  in
  Alcotest.(check string) "to_source" "1" (Syntax.to_source new_root);
  Alcotest.(check int) "child count" 1 (Array.length (Syntax.children_array new_root))
;;

let test_splice_invalid_args () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  (try
     ignore (Syntax.splice_children cache r ~at:99 ~remove:0 []);
     Alcotest.fail "at out of range must raise"
   with
   | Invalid_argument _ -> ());
  try
    ignore (Syntax.splice_children cache r ~at:0 ~remove:99 []);
    Alcotest.fail "remove out of range must raise"
  with
  | Invalid_argument _ -> ()
;;

(* A splice deep in the tree, so the spine rebuild and the off-spine share both
   get exercised. *)
let test_splice_deep () =
  let cache = Cache.create () in
  let g = build_arith4 ~cache () in
  let old_rhs_g =
    let what = "node/token/node" in
    let cs = Green.children_array g in
    arity what 3 cs;
    g_node what cs 2
  in
  let r = Syntax.of_root g in
  let lhs =
    match (Syntax.children_array r).(0) with
    | Syntax.Node c -> c
    | Syntax.Token _ -> Alcotest.fail "expected node child"
  in
  (* In the LHS BIN_EXPR(1,+,2), swap `2` for `7`. *)
  let seven_t = Green.mk_token cache ~kind:K.int_lit ~text:"7" in
  let { Syntax.root = new_root; self = new_lhs } =
    Syntax.splice_children cache lhs ~at:2 ~remove:1 [ Green.Token seven_t ]
  in
  Alcotest.(check string) "to_source" "1+7+3+4" (Syntax.to_source new_root);
  let new_rhs_g =
    let what = "node/token/node" in
    let cs = Green.children_array (Syntax.green new_root) in
    arity what 3 cs;
    g_node what cs 2
  in
  Helpers.same "off-spine RHS share preserved" old_rhs_g new_rhs_g;
  Alcotest.(check int) "new_lhs kind" K.bin_expr (Syntax.kind new_lhs);
  Alcotest.(check string) "new_lhs source" "1+7" (Syntax.to_source new_lhs)
;;

(* The cursor-based helpers, [replace_child] and [splice_at]. What they add over
   [splice_children] is the resolution step, so that is what the next four cases
   exercise: token cursor, node cursor, insert-before, root rejection. *)
let test_replace_child_token () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  let plus_cursor =
    match (Syntax.children_array r).(1) with
    | Syntax.Token t -> t
    | Syntax.Node _ -> Alcotest.fail "expected token child"
  in
  let minus_t = Green.mk_token cache ~kind:K.plus ~text:"-" in
  let { Syntax.root = new_root; self = _ } =
    Syntax.replace_child cache (Syntax.Token plus_cursor) (Green.Token minus_t)
  in
  Alcotest.(check string) "to_source" "1-1" (Syntax.to_source new_root)
;;

let test_replace_child_node () =
  let cache = Cache.create () in
  let g = build_arith4 ~cache () in
  let r = Syntax.of_root g in
  let lhs_cursor =
    match (Syntax.children_array r).(0) with
    | Syntax.Node c -> c
    | Syntax.Token _ -> Alcotest.fail "expected node child"
  in
  let b = Builder.create ~cache () in
  Builder.start_node b K.bin_expr;
  Builder.token b K.int_lit "9";
  Builder.token b K.plus "+";
  Builder.token b K.int_lit "9";
  Builder.finish_node b;
  let replacement = Builder.finish b in
  let { Syntax.root = new_root; self = _ } =
    Syntax.replace_child cache (Syntax.Node lhs_cursor) (Green.Node replacement)
  in
  Alcotest.(check string) "to_source" "9+9+3+4" (Syntax.to_source new_root)
;;

let test_splice_at_insert_before () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  (* Insert "0+" immediately before the first `1` token. *)
  let lhs_cursor =
    match (Syntax.children_array r).(0) with
    | Syntax.Token t -> t
    | Syntax.Node _ -> Alcotest.fail "expected token child"
  in
  let zero = Green.mk_token cache ~kind:K.int_lit ~text:"0" in
  let plus = Green.mk_token cache ~kind:K.plus ~text:"+" in
  let { Syntax.root = new_root; _ } =
    Syntax.splice_at
      cache
      (Syntax.Token lhs_cursor)
      ~remove:0
      [ Green.Token zero; Green.Token plus ]
  in
  Alcotest.(check string) "to_source" "0+1+1" (Syntax.to_source new_root)
;;

let test_replace_child_root_rejected () =
  let cache = Cache.create () in
  let g = build_one_plus_one ~cache () in
  let r = Syntax.of_root g in
  let dummy = Green.mk_token cache ~kind:K.int_lit ~text:"9" in
  try
    ignore (Syntax.replace_child cache (Syntax.Node r) (Green.Token dummy));
    Alcotest.fail "replace_child on a root cursor must be rejected"
  with
  | Failure _ -> ()
;;

let () =
  Alcotest.run
    "siesta"
    [ ( "hash-cons foundations"
      , [ Alcotest.test_case "token tag identity" `Quick test_token_tag_identity
        ; Alcotest.test_case "token distinct text" `Quick test_token_distinct_text
        ; Alcotest.test_case "token distinct kind" `Quick test_token_distinct_kind
        ; Alcotest.test_case "node tag identity" `Quick test_node_tag_identity
        ; Alcotest.test_case "node distinct kind" `Quick test_node_distinct_kind
        ; Alcotest.test_case "node distinct arity" `Quick test_node_distinct_arity
        ; Alcotest.test_case "node payload distinct" `Quick test_node_payload_distinct
        ; Alcotest.test_case
            "node payload zero shared"
            `Quick
            test_node_payload_zero_shared
        ; Alcotest.test_case "1+1 token shared" `Quick test_one_plus_one_token_shared
        ; Alcotest.test_case "(1+1)+(1+1) inner shared" `Quick test_inner_bin_shared
        ; Alcotest.test_case "off-spine sharing" `Quick test_off_spine_sharing
        ; Alcotest.test_case
            "weak collects unreferenced"
            `Quick
            test_weak_collects_unreferenced
        ; Alcotest.test_case
            "weak held entries survive"
            `Quick
            test_weak_held_entries_survive
        ; Alcotest.test_case "cache clear" `Quick test_cache_clear
        ; Alcotest.test_case
            "hash distribution tokens"
            `Quick
            test_hash_distribution_tokens
        ; Alcotest.test_case "hash distribution nodes" `Quick test_hash_distribution_nodes
        ] )
    ; ( "green surface + builder"
      , [ Alcotest.test_case "builder 1+1" `Quick test_builder_one_plus_one
        ; Alcotest.test_case "builder text_len" `Quick test_builder_text_len
        ; Alcotest.test_case
            "builder inner bin shared"
            `Quick
            test_builder_inner_bin_shared
        ; Alcotest.test_case "cross-builder sharing" `Quick test_cross_builder_sharing
        ; Alcotest.test_case "disjoint caches" `Quick test_disjoint_caches
        ; Alcotest.test_case "builder checkpoint" `Quick test_builder_checkpoint
        ; Alcotest.test_case
            "checkpoint rejected after frame closes"
            `Quick
            test_checkpoint_rejected_after_frame_closes
        ; Alcotest.test_case
            "checkpoint rejected when buried"
            `Quick
            test_checkpoint_rejected_when_buried
        ; Alcotest.test_case
            "checkpoint rejected when position stale"
            `Quick
            test_checkpoint_rejected_when_position_stale
        ; Alcotest.test_case
            "checkpoint inner-first still wraps"
            `Quick
            test_checkpoint_inner_first_still_wraps
        ; Alcotest.test_case
            "checkpoint reuse left-assoc"
            `Quick
            test_checkpoint_reuse_left_assoc
        ; Alcotest.test_case "deep tree" `Quick test_deep_tree
        ; Alcotest.test_case "pp" `Quick test_pp
        ; Alcotest.test_case "builder errors" `Quick test_builder_errors
        ] )
    ; ( "syntax (red) layer + replace"
      , [ Alcotest.test_case "syntax to_source" `Quick test_syntax_to_source
        ; Alcotest.test_case "text_range" `Quick test_text_range
        ; Alcotest.test_case "parent navigation" `Quick test_parent_navigation
        ; Alcotest.test_case "children memoization" `Quick test_children_memoization
        ; Alcotest.test_case "equal vs same_tree" `Quick test_equal_vs_same_tree
        ; Alcotest.test_case "replace off-spine share" `Quick test_replace_off_spine_share
        ; Alcotest.test_case "replace at root" `Quick test_replace_at_root
        ; Alcotest.test_case
            "replace idempotent under same green"
            `Quick
            test_replace_idempotent_under_same_green
        ; Alcotest.test_case "splice insert" `Quick test_splice_insert
        ; Alcotest.test_case "splice remove" `Quick test_splice_remove
        ; Alcotest.test_case "splice invalid args" `Quick test_splice_invalid_args
        ; Alcotest.test_case "splice deep" `Quick test_splice_deep
        ; Alcotest.test_case "replace_child token" `Quick test_replace_child_token
        ; Alcotest.test_case "replace_child node" `Quick test_replace_child_node
        ; Alcotest.test_case "splice_at insert before" `Quick test_splice_at_insert_before
        ; Alcotest.test_case
            "replace_child root rejected"
            `Quick
            test_replace_child_root_rejected
        ] )
    ]
;;
