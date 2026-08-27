(* Property-based tests for the siesta library.

   Everything here runs off one recursive [shape] ADT. Interpreters turn a
   shape into a green tree, through [Builder] or through [Green.mk_*] directly,
   or compute the source text it implies, and the properties compare the two.

   The census at the bottom checks the corpus is still producing the shapes
   these properties need. *)

open Siesta
module Gen = QCheck2.Gen
module Test = QCheck2.Test

(* -- kinds ----------------------------------------------------------------- *)

(* A small fixed kind set for the generator. Token kinds split variable text
   ([lit], [bad]) from fixed ([ws], [op]); node kinds take any arity. *)
module K = struct
  let lit = 1 (* var text: digits *)
  let bad = 2 (* var text: arbitrary chars *)
  let ws = 3 (* fixed " " *)
  let op = 4 (* fixed "+" *)

  (* Zero-width token. A lossless CST carries these sooner or later, an empty
     error-recovery token or a delimiter the parser synthesised but never saw,
     and every offset path has to cope with the empty half-open range. *)
  let emp = 5
  let leaf = 10
  let bin = 11
  let block = 12
  let node_kinds = [ leaf; bin; block ]
end

(* -- shape ADT ------------------------------------------------------------- *)

(* [N_node] carries a payload. Payload is part of hash-cons identity, so a
   generator that leaves it at 0 throughout makes every property below blind to
   it: dropping the [~payload] argument from [Syntax.rebuild_spine] used to
   leave the whole suite green. *)
type shape =
  | T_lit of string
  | T_bad of string
  | T_ws
  | T_op
  | T_emp
  | N_node of int * int * shape list (* kind, payload, children *)

let rec pp_shape ppf = function
  | T_lit s -> Format.fprintf ppf "(LIT %S)" s
  | T_bad s -> Format.fprintf ppf "(BAD %S)" s
  | T_ws -> Format.fprintf ppf "WS"
  | T_op -> Format.fprintf ppf "OP"
  | T_emp -> Format.fprintf ppf "EMP"
  | N_node (k, p, cs) ->
    Format.fprintf ppf "@[<hv 2>(N%d/p%d" k p;
    List.iter (fun c -> Format.fprintf ppf "@ %a" pp_shape c) cs;
    Format.fprintf ppf ")@]"
;;

let print_shape s = Format.asprintf "%a" pp_shape s

(* -- generators ------------------------------------------------------------ *)

let gen_digit_text = Gen.map string_of_int (Gen.int_range 0 9999)

let gen_bad_text =
  (* Short lowercase runs, so the text differs from the fixed-text kinds. *)
  Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 4)
;;

let gen_token =
  Gen.oneof_weighted
    [ 3, Gen.map (fun s -> T_lit s) gen_digit_text
    ; 1, Gen.map (fun s -> T_bad s) gen_bad_text
    ; 2, Gen.return T_ws
    ; 2, Gen.return T_op
    ; 1, Gen.return T_emp
    ]
;;

let gen_node_kind = Gen.oneof_list K.node_kinds

(* A deliberately small range. Payloads have to collide often enough that
   hash-consing still shares nodes, which the sharing properties need, and be
   non-zero often enough that a dropped payload shows up. *)
let gen_payload = Gen.int_range 0 3

(* The single-child alternative biases the odd draw towards a deep narrow
   chain. A tree wide at every level almost never gets far down, and the spine
   walks in [replace] and [Ptr] are the code that cares about depth. *)
let rec gen_shape depth =
  if depth <= 0
  then gen_token
  else
    Gen.oneof_weighted
      [ 2, gen_token
      ; ( 3
        , Gen.map3
            (fun k p cs -> N_node (k, p, cs))
            gen_node_kind
            gen_payload
            (Gen.list_size (Gen.int_range 0 4) (gen_shape (depth - 1))) )
      ; ( 1
        , Gen.map3
            (fun k p c -> N_node (k, p, [ c ]))
            gen_node_kind
            gen_payload
            (gen_shape (depth - 1)) )
      ]
;;

(* The root is always a node, since Builder wants a [start_node] before any
   token. *)
let gen_root =
  Gen.map3
    (fun k p cs -> N_node (k, p, cs))
    gen_node_kind
    gen_payload
    (Gen.list_size (Gen.int_range 1 5) (gen_shape 4))
;;

(* -- interpreters ---------------------------------------------------------- *)

let token_text = function
  | T_lit s | T_bad s -> s
  | T_ws -> " "
  | T_op -> "+"
  | T_emp -> ""
  | N_node _ -> assert false
;;

let token_kind = function
  | T_lit _ -> K.lit
  | T_bad _ -> K.bad
  | T_ws -> K.ws
  | T_op -> K.op
  | T_emp -> K.emp
  | N_node _ -> assert false
;;

let build_via_builder cache shape =
  let b = Builder.create ~cache () in
  let rec go = function
    | N_node (k, p, cs) ->
      Builder.start_node b ~payload:p k;
      List.iter go cs;
      Builder.finish_node b
    | (T_lit _ | T_bad _ | T_ws | T_op | T_emp) as tok ->
      Builder.token b (token_kind tok) (token_text tok)
  in
  go shape;
  Builder.finish b
;;

let expected_source shape =
  let buf = Buffer.create 64 in
  let rec go = function
    | N_node (_, _, cs) -> List.iter go cs
    | (T_lit _ | T_bad _ | T_ws | T_op | T_emp) as tok ->
      Buffer.add_string buf (token_text tok)
  in
  go shape;
  Buffer.contents buf
;;

(* Direct construction through [Green.mk_*], for the property that says the
   builder lands on the same value as building by hand. *)
let rec build_via_green cache = function
  | N_node (k, p, cs) ->
    let children = Array.of_list (List.map (build_child_via_green cache) cs) in
    Helpers.mk_node cache ~payload:p k children
  | T_lit _ | T_bad _ | T_ws | T_op | T_emp ->
    failwith "build_via_green: root must be a node"

and build_child_via_green cache = function
  | N_node _ as n -> Green.Node (build_via_green cache n)
  | (T_lit _ | T_bad _ | T_ws | T_op | T_emp) as tok ->
    Green.Token (Helpers.mk_tok cache (token_kind tok) (token_text tok))
;;

(* -- source roundtrip ------------------------------------------------------ *)

let prop_source_roundtrip =
  Test.make
    ~name:"Green.to_source matches expected text from shape"
    ~count:1000
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let root = build_via_builder cache shape in
       String.equal (Green.to_source root) (expected_source shape))
;;

(* -- text_range partitions ------------------------------------------------- *)

(* The children's text_ranges have to cover the parent's contiguously, with no
   gaps, no overlaps and exact endpoints. *)
let rec text_range_partitions_ok cursor =
  let lo, hi = Syntax.text_range cursor in
  let cs = Syntax.children_array cursor in
  if Array.length cs = 0
  then hi - lo = Green.text_len (Syntax.green cursor)
  else (
    let child_range = function
      | Syntax.Node c -> Syntax.text_range c
      | Syntax.Token t -> Syntax.Token.text_range t
    in
    let contiguous = ref true in
    let prev = ref lo in
    Array.iter
      (fun c ->
         let clo, chi = child_range c in
         if clo <> !prev then contiguous := false;
         prev := chi)
      cs;
    !contiguous
    && !prev = hi
    && Array.for_all
         (function
           | Syntax.Node child -> text_range_partitions_ok child
           | Syntax.Token _ -> true)
         cs)
;;

let prop_text_range_partition =
  Test.make
    ~name:"text_range partitions cover parents exactly"
    ~count:1000
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let root = build_via_builder cache shape in
       text_range_partitions_ok (Syntax.of_root root))
;;

(* -- hash-cons determinism ------------------------------------------------- *)

(* Same shape, same cache, two builds, one root. Hash-consing has to
   canonicalise the output of independent build sequences. *)
let prop_hashcons_builder_idempotent =
  Test.make
    ~name:"same shape via builder twice yields == root"
    ~count:1000
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let r1 = build_via_builder cache shape in
       let r2 = build_via_builder cache shape in
       r1 == r2)
;;

(* Builder against direct [Green.mk_*] construction. Both paths through one
   cache have to land on the same hash-consed value. *)
let prop_builder_eq_green =
  Test.make
    ~name:"builder == direct Green construction"
    ~count:1000
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let r1 = build_via_builder cache shape in
       let r2 = build_via_green cache shape in
       r1 == r2)
;;

(* -- off-spine physical sharing -------------------------------------------- *)

(* Every path from the root that lands on a node, as raw child indices counting
   trivia. The empty path is the root itself. *)
let collect_node_paths (root : Green.node) : int list list =
  let acc = ref [] in
  let rec walk path node =
    acc := path :: !acc;
    let cs = Green.children_array node in
    Array.iteri
      (fun i c ->
         match c with
         | Green.Node child -> walk (path @ [ i ]) child
         | Green.Token _ -> ())
      cs
  in
  walk [] root;
  !acc
;;

let rec navigate_cursor cursor = function
  | [] -> cursor
  | i :: rest ->
    (match Syntax.nth_child cursor i with
     | Some (Syntax.Node c) -> navigate_cursor c rest
     | Some (Syntax.Token _) | None ->
       failwith "navigate_cursor: path does not lead to a node")
;;

(* Compare two green trees along a spine of child indices. Everything off the
   spine has to be [==] to the original; the node the path ends at is the
   replacement target and may differ entirely. *)
let rec off_spine_shared old_node new_node = function
  | [] -> true (* terminal target, replacement allowed to differ *)
  | i :: rest ->
    let old_cs = Green.children_array old_node in
    let new_cs = Green.children_array new_node in
    if Array.length old_cs <> Array.length new_cs
    then false
    else (
      let ok = ref true in
      Array.iteri
        (fun j old_c ->
           let new_c = new_cs.(j) in
           if j = i
           then (
             (* On the spine, so recurse. *)
             match old_c, new_c with
             | Green.Node o, Green.Node n ->
               if not (off_spine_shared o n rest) then ok := false
             | Green.Node _, Green.Token _
             | Green.Token _, Green.Node _
             | Green.Token _, Green.Token _ -> ok := false (* a spine slot is a node *))
           else (
             (* Off the spine, so it has to be the same OCaml value. *)
             match old_c, new_c with
             | Green.Node o, Green.Node n -> if not (o == n) then ok := false
             | Green.Token o, Green.Token n -> if not (o == n) then ok := false
             | Green.Node _, Green.Token _ | Green.Token _, Green.Node _ -> ok := false))
        old_cs;
      !ok)
;;

let prop_off_spine_sharing =
  Test.make
    ~name:"Syntax.replace preserves off-spine physical sharing"
    ~count:500
    ~print:(fun (orig, picker, repl) ->
      Printf.sprintf
        "orig=%s\npicker=%d\nrepl=%s"
        (print_shape orig)
        picker
        (print_shape repl))
    Gen.(triple gen_root (int_range 0 1_000_000) gen_root)
    (fun (orig_shape, picker, repl_shape) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let repl = build_via_builder cache repl_shape in
       let cursor = Syntax.of_root orig in
       let target = navigate_cursor cursor path in
       let result = Syntax.replace cache target repl in
       let new_root = Syntax.green result.Syntax.root in
       off_spine_shared orig new_root path)
;;

(* -- text_len matches source length ---------------------------------------- *)

(* [Green.text_len] is precomputed at construction, so it has to stay in step
   with [Green.to_source]. Checked at every node: a pair of per-node errors that
   cancel out still adds up to the right total at the root. *)
let rec all_green_nodes acc (n : Green.node) =
  Array.fold_left
    (fun acc c ->
       match c with
       | Green.Node child -> all_green_nodes acc child
       | Green.Token _ -> acc)
    (n :: acc)
    (Green.children_array n)
;;

let prop_text_len_matches_source =
  Test.make
    ~name:"Green.text_len = String.length (Green.to_source _) at every node"
    ~count:1000
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let root = build_via_builder cache shape in
       List.for_all
         (fun n -> Green.text_len n = String.length (Green.to_source n))
         (all_green_nodes [] root))
;;

(* -- parent / child round-trip --------------------------------------------- *)

(* For any non-root cursor [c], [parent c] and that parent's
   [nth_child c.index_in_parent] give back the very same OCaml record. That is
   the [Syntax] memoization promise. *)

let prop_parent_child_roundtrip =
  Test.make
    ~name:"parent + nth_child round-trips by physical equality"
    ~count:1000
    ~print:(fun (s, i) -> Printf.sprintf "shape=%s picker=%d" (print_shape s) i)
    Gen.(pair gen_root (int_range 0 1_000_000))
    (fun (shape, picker) ->
       let cache = Cache.create () in
       let root = build_via_builder cache shape in
       let paths = collect_node_paths root in
       let path = List.nth paths (picker mod List.length paths) in
       let cursor = navigate_cursor (Syntax.of_root root) path in
       match Syntax.parent cursor with
       | None -> path = []
       | Some p ->
         (match Syntax.nth_child p (Syntax.index_in_parent cursor) with
          | Some (Syntax.Node c) -> c == cursor
          | Some (Syntax.Token _) | None -> false))
;;

(* -- idempotent replace ---------------------------------------------------- *)

(* Replacing a node with its own green rebuilds the spine out of children
   arrays identical to the originals, so hash-consing hands back the existing
   green at every level and the new root is the old one. *)

let prop_idempotent_replace =
  Test.make
    ~name:"replace cache c (green c) yields tag-equal root"
    ~count:500
    ~print:(fun (s, i) -> Printf.sprintf "shape=%s picker=%d" (print_shape s) i)
    Gen.(pair gen_root (int_range 0 1_000_000))
    (fun (shape, picker) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let result = Syntax.replace cache target (Syntax.green target) in
       Syntax.green result.Syntax.root == orig)
;;

(* -- to_source after replace ----------------------------------------------- *)

(* The new tree's source is the old source with the target's span swapped for
   the replacement's text. Source, text_range and replace all in one property,
   so a failure names whichever piece broke. *)

let prop_to_source_after_replace =
  Test.make
    ~name:"to_source after replace = orig[0..lo] + repl + orig[hi..]"
    ~count:500
    ~print:(fun (orig, picker, repl) ->
      Printf.sprintf
        "orig=%s\npicker=%d\nrepl=%s"
        (print_shape orig)
        picker
        (print_shape repl))
    Gen.(triple gen_root (int_range 0 1_000_000) gen_root)
    (fun (orig_shape, picker, repl_shape) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let repl = build_via_builder cache repl_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let lo, hi = Syntax.text_range target in
       let result = Syntax.replace cache target repl in
       let new_src = Green.to_source (Syntax.green result.Syntax.root) in
       let orig_src = Green.to_source orig in
       let repl_src = Green.to_source repl in
       let expected =
         String.sub orig_src 0 lo
         ^ repl_src
         ^ String.sub orig_src hi (String.length orig_src - hi)
       in
       String.equal new_src expected)
;;

(* -- splice helpers -------------------------------------------------------- *)

(* One int carries both [at] and [remove], each reduced modulo its bound. Keeps
   the generator simple and the shrinker honest. *)
let pick_splice_params target_g splice_picker =
  let n = Green.num_children target_g in
  let at = splice_picker / 1009 mod (n + 1) in
  let remove = splice_picker mod 1009 mod (n - at + 1) in
  at, remove
;;

(* Inserts are a short list of shapes, tokens and nodes alike. *)
let gen_inserts = Gen.list_size (Gen.int_range 0 3) (gen_shape 2)

let print_splice (orig, picker, sp, inserts) =
  Printf.sprintf
    "orig=%s\npicker=%d sp=%d\ninserts=[%s]"
    (print_shape orig)
    picker
    sp
    (String.concat "; " (List.map print_shape inserts))
;;

(* -- splice arithmetic ----------------------------------------------------- *)

(* After [splice_children ~at ~remove inserts] the child count is
   [old - remove + |inserts|], and text_len moves by the same bookkeeping. *)

let prop_splice_arithmetic =
  Test.make
    ~name:"splice_children: child count + text_len math"
    ~count:500
    ~print:print_splice
    Gen.(quad gen_root (int_range 0 1_000_000) (int_range 0 1_000_000) gen_inserts)
    (fun (orig_shape, picker, splice_picker, inserts_shapes) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let target_g = Syntax.green target in
       let at, remove = pick_splice_params target_g splice_picker in
       let inserts = List.map (build_child_via_green cache) inserts_shapes in
       let result = Syntax.splice_children cache target ~at ~remove inserts in
       let new_target_g = Syntax.green result.Syntax.self in
       let removed_text_len = ref 0 in
       for i = at to at + remove - 1 do
         removed_text_len
         := !removed_text_len
            + Green.child_text_len (Option.get (Green.nth_child target_g i))
       done;
       let insert_text_len = Green.sum_text_len (Array.of_list inserts) in
       let expected_count = Green.num_children target_g - remove + List.length inserts in
       let expected_text_len =
         Green.text_len target_g - !removed_text_len + insert_text_len
       in
       Green.num_children new_target_g = expected_count
       && Green.text_len new_target_g = expected_text_len)
;;

(* -- splice off-spine sharing ---------------------------------------------- *)

(* The same sharing guarantee as [replace]: every off-spine green descendant is
   [==] to the original. *)

let prop_splice_off_spine_sharing =
  Test.make
    ~name:"Syntax.splice_children preserves off-spine =="
    ~count:500
    ~print:print_splice
    Gen.(quad gen_root (int_range 0 1_000_000) (int_range 0 1_000_000) gen_inserts)
    (fun (orig_shape, picker, splice_picker, inserts_shapes) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let target_g = Syntax.green target in
       let at, remove = pick_splice_params target_g splice_picker in
       let inserts = List.map (build_child_via_green cache) inserts_shapes in
       let result = Syntax.splice_children cache target ~at ~remove inserts in
       let new_root = Syntax.green result.Syntax.root in
       off_spine_shared orig new_root path)
;;

(* -- to_source after splice ------------------------------------------------ *)

(* The new source is the old one with the removed children's text swapped for
   the inserts'. The splice point comes from summing text_len over the target's
   children up to [at]. *)

let prop_to_source_after_splice =
  Test.make
    ~name:"to_source after splice = expected text"
    ~count:500
    ~print:print_splice
    Gen.(quad gen_root (int_range 0 1_000_000) (int_range 0 1_000_000) gen_inserts)
    (fun (orig_shape, picker, splice_picker, inserts_shapes) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let target_g = Syntax.green target in
       let at, remove = pick_splice_params target_g splice_picker in
       let inserts = List.map (build_child_via_green cache) inserts_shapes in
       let target_lo, _ = Syntax.text_range target in
       let prefix_len = ref 0 in
       for i = 0 to at - 1 do
         prefix_len
         := !prefix_len + Green.child_text_len (Option.get (Green.nth_child target_g i))
       done;
       let removed_len = ref 0 in
       for i = at to at + remove - 1 do
         removed_len
         := !removed_len + Green.child_text_len (Option.get (Green.nth_child target_g i))
       done;
       let splice_lo = target_lo + !prefix_len in
       let splice_hi = splice_lo + !removed_len in
       let inserts_src =
         let buf = Buffer.create 16 in
         List.iter
           (function
             | Green.Token t -> Buffer.add_string buf (Green.Token.text t)
             | Green.Node n -> Buffer.add_string buf (Green.to_source n))
           inserts;
         Buffer.contents buf
       in
       let result = Syntax.splice_children cache target ~at ~remove inserts in
       let new_src = Green.to_source (Syntax.green result.Syntax.root) in
       let orig_src = Green.to_source orig in
       let expected =
         String.sub orig_src 0 splice_lo
         ^ inserts_src
         ^ String.sub orig_src splice_hi (String.length orig_src - splice_hi)
       in
       String.equal new_src expected)
;;

(* -- checkpoint left-assoc against direct nesting --------------------------- *)

(* A left-associative chain of [bin]-wrapped tokens, built twice: once by
   capturing a single checkpoint at the start of the block and re-wrapping for
   each RHS, which is the calc parser idiom, and once by nesting directly.
   Through a shared cache the two roots have to be [==]. *)

let build_chain_via_checkpoint cache (head, rhss) =
  let b = Builder.create ~cache () in
  Builder.start_node b K.block;
  let cp = Builder.checkpoint b in
  Builder.token b K.lit head;
  List.iter
    (fun rhs ->
       Builder.start_node_at b cp K.bin;
       Builder.token b K.op "+";
       Builder.token b K.lit rhs;
       Builder.finish_node b)
    rhss;
  Builder.finish_node b;
  Builder.finish b
;;

let build_chain_via_direct cache (head, rhss) =
  let b = Builder.create ~cache () in
  Builder.start_node b K.block;
  let rec emit head rhss_init =
    match rhss_init with
    | [] -> Builder.token b K.lit head
    | _ ->
      let n = List.length rhss_init in
      let last = List.nth rhss_init (n - 1) in
      let init = List.filteri (fun i _ -> i < n - 1) rhss_init in
      Builder.start_node b K.bin;
      emit head init;
      Builder.token b K.op "+";
      Builder.token b K.lit last;
      Builder.finish_node b
  in
  emit head rhss;
  Builder.finish_node b;
  Builder.finish b
;;

let prop_checkpoint_eq_direct =
  Test.make
    ~name:"checkpoint reuse builds left-assoc identically to direct nesting"
    ~count:500
    ~print:(fun (h, rs) -> Printf.sprintf "head=%S rhss=[%s]" h (String.concat ";" rs))
    Gen.(
      pair
        (string_size ~gen:(char_range '0' '9') (int_range 1 3))
        (list_size
           (int_range 0 8)
           (string_size ~gen:(char_range '0' '9') (int_range 1 3))))
    (fun (head, rhss) ->
       let cache = Cache.create () in
       let r1 = build_chain_via_checkpoint cache (head, rhss) in
       let r2 = build_chain_via_direct cache (head, rhss) in
       r1 == r2)
;;

(* -- same_tree agrees with reachability ------------------------------------- *)

(* [same_tree] is how you ask whether two cursors belong to the same
   navigation. Two claims here: every cursor reachable from a root shares that
   root, and cursors from a separate [of_root] on the very same green never do.
   The second is what stops "always true" from passing. *)

let prop_same_tree =
  Test.make
    ~name:"same_tree agrees with reachable-from-the-same-root"
    ~count:500
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let green = build_via_builder cache shape in
       let a = Syntax.of_root green in
       (* A second navigation over literally the same green value, which is the
          sharpest case available. *)
       let b = Syntax.of_root green in
       let all_a = List.of_seq (Syntax.descendants a) in
       let all_b = List.of_seq (Syntax.descendants b) in
       List.for_all (fun c -> Syntax.same_tree c a) all_a
       && List.for_all (fun c -> Syntax.same_tree c b) all_b
       && List.for_all
            (fun ca -> List.for_all (fun cb -> not (Syntax.same_tree ca cb)) all_b)
            all_a)
;;

(* -- occurrences stay distinct ---------------------------------------------- *)

(* Hash-consing makes two identical subtrees one green node, so a [Ptr] carries
   the path and has to separate every position in the tree.

   Both halves are here on purpose. "All ptrs are distinct" on its own is
   satisfied by a [Ptr] that invents a fresh id every call, so the resolve half
   sits alongside it as the expected value. *)

module PtrSet = Set.Make (struct
    type t = Syntax.Ptr.t

    let compare = Syntax.Ptr.compare
  end)

let prop_ptr_separates_occurrences =
  Test.make
    ~name:"distinct node positions get distinct ptrs, each resolving back"
    ~count:500
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let root = Syntax.of_root (build_via_builder cache shape) in
       let cursors = List.of_seq (Syntax.descendants root) in
       let ptrs = List.map Syntax.Ptr.of_node cursors in
       let distinct = PtrSet.cardinal (PtrSet.of_list ptrs) = List.length ptrs in
       let resolves =
         List.for_all2
           (fun c p ->
              match Syntax.Ptr.resolve_node root p with
              | Some c' -> c' == c
              | None -> false)
           cursors
           ptrs
       in
       distinct && resolves)
;;

(* -- payload survives a spine rebuild --------------------------------------- *)

(* [replace] rebuilds every ancestor of the target through [Green.mk_node], so
   each one has to be handed its own kind and its own payload. Drop the payload
   argument in [Syntax.rebuild_spine] and the ancestors all come back with
   payload 0, which nothing else in the suite would notice. *)

let prop_replace_preserves_payload =
  Test.make
    ~name:"replace preserves kind and payload on every rebuilt ancestor"
    ~count:500
    ~print:(fun (s, i, r) ->
      Printf.sprintf "orig=%s\npicker=%d\nrepl=%s" (print_shape s) i (print_shape r))
    Gen.(triple gen_root (int_range 0 1_000_000) gen_root)
    (fun (orig_shape, picker, repl_shape) ->
       let cache = Cache.create () in
       let orig = build_via_builder cache orig_shape in
       let repl = build_via_builder cache repl_shape in
       let paths = collect_node_paths orig in
       let path = List.nth paths (picker mod List.length paths) in
       let target = navigate_cursor (Syntax.of_root orig) path in
       let result = Syntax.replace cache target repl in
       let new_root = Syntax.green result.Syntax.root in
       (* Walk the spine in both trees together. Every node strictly above the
          target keeps its kind and payload, and the walk stops before the
          target itself, which is the replacement. *)
       let rec spine_ok old_n new_n = function
         | [] -> true
         | i :: rest ->
           Green.kind old_n = Green.kind new_n
           && Green.payload old_n = Green.payload new_n
           &&
             (match Green.nth_child old_n i, Green.nth_child new_n i with
             | Some (Green.Node o), Some (Green.Node n) -> spine_ok o n rest
             | Some (Green.Token _), _ | _, Some (Green.Token _) | None, _ | _, None ->
               false)
       in
       spine_ok orig new_root path)
;;

(* -- token cursors round-trip too ------------------------------------------- *)

(* The mirror of [prop_parent_child_roundtrip] for tokens, which have their own
   cursor type and so are untouched by the node property. *)

let prop_token_parent_roundtrip =
  Test.make
    ~name:"Token.parent + nth_child round-trips by physical equality"
    ~count:500
    ~print:print_shape
    gen_root
    (fun shape ->
       let cache = Cache.create () in
       let root = Syntax.of_root (build_via_builder cache shape) in
       Seq.for_all
         (fun node ->
            Array.for_all
              (function
                | Syntax.Node _ -> true
                | Syntax.Token tc ->
                  let p = Syntax.Token.parent tc in
                  p == node
                  &&
                    (match Syntax.nth_child p (Syntax.Token.index_in_parent tc) with
                    | Some (Syntax.Token tc') -> tc' == tc
                    | Some (Syntax.Node _) | None -> false))
              (Syntax.children_array node))
         (Syntax.descendants root))
;;

(* -- what the corpus actually produced -------------------------------------- *)

(* Iteration counts say nothing about coverage. A property can run 1,000 times
   and never once meet the shape it claims to forbid, which is how a law ends up
   green and empty. So this counts the kinds of tree the generator really emits
   and fails if any class has dried up.

   The floors are deliberately slack. They catch a generator that has stopped
   producing a class altogether, and should not need touching when the weights
   move a bit. *)

let test_corpus_census () =
  let n = 2000 in
  let rand = Random.State.make [| 0x51E57A |] in
  let with_payload = ref 0
  and with_zero_width_node = ref 0
  and with_empty_token = ref 0
  and with_shared_green = ref 0
  and with_empty_node = ref 0
  and deep = ref 0
  and max_depth = ref 0 in
  for _ = 1 to n do
    let shape = Gen.generate1 ~rand gen_root in
    let cache = Cache.create () in
    let green = build_via_builder cache shape in
    let nodes = all_green_nodes [] green in
    let bump c b = if b then incr c in
    bump with_payload (List.exists (fun x -> Green.payload x <> 0) nodes);
    bump with_zero_width_node (List.exists (fun x -> Green.text_len x = 0) nodes);
    bump with_empty_node (List.exists (fun x -> Green.num_children x = 0) nodes);
    bump
      with_empty_token
      (List.exists
         (fun x ->
            Array.exists
              (function
                | Green.Token t -> String.equal (Green.Token.text t) ""
                | Green.Node _ -> false)
              (Green.children_array x))
         nodes);
    (* Two node positions holding one green value, the shape
       [prop_ptr_separates_occurrences] is really about. At zero, that property
       has quietly stopped testing anything. *)
    let root_cursor = Syntax.of_root green in
    let cursors = List.of_seq (Syntax.descendants root_cursor) in
    let tags = List.map (fun c -> Green.tag (Syntax.green c)) cursors in
    let uniq = List.sort_uniq compare tags in
    bump with_shared_green (List.length uniq < List.length tags);
    let d =
      List.fold_left (fun acc c -> max acc (Seq.length (Syntax.ancestors c))) 0 cursors
    in
    if d > !max_depth then max_depth := d;
    bump deep (d >= 5)
  done;
  let at_least what floor got =
    Alcotest.(check bool)
      (Printf.sprintf "%s: %d/%d trees (floor %d)" what got n floor)
      true
      (got >= floor)
  in
  at_least "non-zero payload somewhere" (n / 4) !with_payload;
  at_least "zero-width node somewhere" (n / 20) !with_zero_width_node;
  at_least "empty-text token somewhere" (n / 10) !with_empty_token;
  at_least "childless node somewhere" (n / 10) !with_empty_node;
  at_least "two positions sharing one green" (n / 10) !with_shared_green;
  at_least "spine 5 deep or more" (n / 20) !deep;
  Alcotest.(check bool)
    (Printf.sprintf "deepest spine seen was %d" !max_depth)
    true
    (!max_depth >= 5)
;;

(* -- entry point ----------------------------------------------------------- *)

let () =
  Alcotest.run
    "props"
    [ ( "source"
      , [ Helpers.qcheck prop_source_roundtrip
        ; Helpers.qcheck prop_text_len_matches_source
        ; Helpers.qcheck prop_text_range_partition
        ] )
    ; ( "hash-cons"
      , [ Helpers.qcheck prop_hashcons_builder_idempotent
        ; Helpers.qcheck prop_builder_eq_green
        ; Helpers.qcheck prop_checkpoint_eq_direct
        ] )
    ; ( "navigation"
      , [ Helpers.qcheck prop_parent_child_roundtrip
        ; Helpers.qcheck prop_token_parent_roundtrip
        ; Helpers.qcheck prop_same_tree
        ; Helpers.qcheck prop_ptr_separates_occurrences
        ] )
    ; ( "replace"
      , [ Helpers.qcheck prop_idempotent_replace
        ; Helpers.qcheck prop_off_spine_sharing
        ; Helpers.qcheck prop_to_source_after_replace
        ; Helpers.qcheck prop_replace_preserves_payload
        ] )
    ; ( "splice"
      , [ Helpers.qcheck prop_splice_arithmetic
        ; Helpers.qcheck prop_splice_off_spine_sharing
        ; Helpers.qcheck prop_to_source_after_splice
        ] )
    ; "corpus", [ Alcotest.test_case "census" `Quick test_corpus_census ]
    ]
;;
