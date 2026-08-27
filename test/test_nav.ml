open Siesta
open Helpers

module K = struct
  let kw = 1
  let id = 2
  let inner = 40
  let body = 30
  let let_ = 20
  let root = 100
end

(* A two-definition fixture whose two [INNER] subtrees are textually identical,
   and therefore one hash-consed green node:

     ROOT                                         [0,18)
     ├── LET                                      [0,9)
     │   ├── KW    "let "                         [0,4)
     │   └── BODY                                 [4,9)
     │       ├── ID    "foo"                      [4,7)
     │       └── INNER                            [7,9)
     │           └── ID "xy"                      [7,9)
     └── LET                                      [9,18)
         ├── KW    "let "                         [9,13)
         └── BODY                                 [13,18)
             ├── ID    "bar"                      [13,16)
             └── INNER                            [16,18)
                 └── ID "xy"                      [16,18) *)
let fixture cache =
  let def name =
    let inner = mk_node cache K.inner [| Green.Token (mk_tok cache K.id "xy") |] in
    let body =
      mk_node cache K.body [| Green.Token (mk_tok cache K.id name); Green.Node inner |]
    in
    mk_node cache K.let_ [| Green.Token (mk_tok cache K.kw "let "); Green.Node body |]
  in
  mk_node cache K.root [| Green.Node (def "foo"); Green.Node (def "bar") |]
;;

let root_cursor cache = Syntax.of_root (fixture cache)

(* [descendants] order is source order, so these indices are stable:
   0 ROOT, 1 LET0, 2 BODY0, 3 INNER0, 4 LET1, 5 BODY1, 6 INNER1. *)
let nodes r = Array.of_seq (Syntax.descendants r)

(* -- Ptr ------------------------------------------------------------------- *)

let test_ptr_distinguishes_occurrences () =
  let r = root_cursor (Cache.create ()) in
  let ns = nodes r in
  let inner0 = ns.(3)
  and inner1 = ns.(6) in
  (* The premise; hash-consing has collapsed them to one green value. *)
  same "identical subtrees are one green node" (Syntax.green inner0) (Syntax.green inner1);
  Alcotest.(check bool)
    "…so green identity cannot tell the occurrences apart"
    true
    (Green.equal (Syntax.green inner0) (Syntax.green inner1));
  (* The point; ptrs can. *)
  let p0 = Syntax.Ptr.of_node inner0
  and p1 = Syntax.Ptr.of_node inner1 in
  Alcotest.(check bool) "ptrs are distinct" false (Syntax.Ptr.equal p0 p1);
  Alcotest.(check bool) "and resolve apart" true (Syntax.Ptr.compare p0 p1 <> 0);
  match Syntax.Ptr.resolve_node r p0, Syntax.Ptr.resolve_node r p1 with
  | Some a, Some b ->
    same "p0 resolves to occurrence 0" a inner0;
    same "p1 resolves to occurrence 1" b inner1
  | _ -> Alcotest.fail "both ptrs should resolve"
;;

let test_ptr_roundtrip_nodes () =
  let r = root_cursor (Cache.create ()) in
  Array.iter
    (fun n ->
       match Syntax.Ptr.resolve_node r (Syntax.Ptr.of_node n) with
       | Some n' -> same "node ptr round-trips to the same cursor" n' n
       | None -> Alcotest.failf "ptr failed to resolve for kind %d" (Syntax.kind n))
    (nodes r)
;;

let test_ptr_roundtrip_tokens () =
  let r = root_cursor (Cache.create ()) in
  let seen = ref 0 in
  Syntax.preorder r ~f:(fun n ->
    Array.iter
      (fun e ->
         match e with
         | Syntax.Token tc ->
           incr seen;
           (match Syntax.Ptr.resolve r (Syntax.Ptr.of_token tc) with
            | Some (Syntax.Token tc') ->
              Alcotest.(check bool)
                "token ptr round-trips"
                true
                (Syntax.Token.equal tc' tc)
            | Some (Syntax.Node _) | None ->
              Alcotest.fail "token ptr should resolve to a token")
         | Syntax.Node _ -> ())
      (Syntax.children_array n);
    Syntax.Descend);
  Alcotest.(check int) "all six tokens checked" 6 !seen
;;

let test_ptr_root () =
  let r = root_cursor (Cache.create ()) in
  let p = Syntax.Ptr.of_node r in
  Alcotest.(check int) "root ptr has depth 0" 0 (Syntax.Ptr.depth p);
  Alcotest.(check int) "root ptr records the root kind" K.root (Syntax.Ptr.kind p);
  match Syntax.Ptr.resolve_node r p with
  | Some r' -> same "root ptr resolves to the root" r' r
  | None -> Alcotest.fail "root ptr should resolve"
;;

(* Every way of being wrong yields [None]. *)
let test_ptr_resolve_is_total () =
  let cache = Cache.create () in
  let r = root_cursor cache in
  let inner0 = (nodes r).(3) in
  let p = Syntax.Ptr.of_node inner0 in
  (* Against a tree that is too shallow for the path. *)
  let shallow =
    Syntax.of_root (mk_node cache K.root [| Green.Token (mk_tok cache K.id "z") |])
  in
  Alcotest.(check bool)
    "out-of-range index resolves to None"
    true
    (Syntax.Ptr.resolve shallow p = None);
  (* Against a tree of the right shape but the wrong kind at the target. *)
  let wrong_kind =
    let inner = mk_node cache (K.inner + 1) [| Green.Token (mk_tok cache K.id "xy") |] in
    let body =
      mk_node cache K.body [| Green.Token (mk_tok cache K.id "foo"); Green.Node inner |]
    in
    let l =
      mk_node cache K.let_ [| Green.Token (mk_tok cache K.kw "let "); Green.Node body |]
    in
    Syntax.of_root (mk_node cache K.root [| Green.Node l |])
  in
  Alcotest.(check bool)
    "kind mismatch resolves to None"
    true
    (Syntax.Ptr.resolve wrong_kind p = None);
  (* A node ptr whose path lands on a token. *)
  let tok_path =
    match Syntax.nth_child (nodes r).(1) 0 with
    | Some (Syntax.Token tc) -> Syntax.Ptr.of_token tc
    | Some (Syntax.Node _) | None -> Alcotest.fail "expected a token child"
  in
  Alcotest.(check bool)
    "resolve_node on a token ptr is None"
    true
    (Syntax.Ptr.resolve_node r tok_path = None)
;;

let test_ptr_is_ancestor () =
  let r = root_cursor (Cache.create ()) in
  let ns = nodes r in
  let p n = Syntax.Ptr.of_node ns.(n) in
  let root = p 0
  and let0 = p 1
  and body0 = p 2
  and inner0 = p 3
  and let1 = p 4 in
  Alcotest.(check bool) "root < inner0" true (Syntax.Ptr.is_ancestor root inner0);
  Alcotest.(check bool) "let0 < body0" true (Syntax.Ptr.is_ancestor let0 body0);
  Alcotest.(check bool) "strict: not reflexive" false (Syntax.Ptr.is_ancestor let0 let0);
  Alcotest.(check bool) "siblings unrelated" false (Syntax.Ptr.is_ancestor let0 let1);
  Alcotest.(check bool) "not upward" false (Syntax.Ptr.is_ancestor inner0 root)
;;

let test_ptr_hash_agrees_with_equal () =
  let r = root_cursor (Cache.create ()) in
  let ps = Array.map Syntax.Ptr.of_node (nodes r) in
  Array.iter
    (fun a ->
       Array.iter
         (fun b ->
            if Syntax.Ptr.equal a b
            then
              Alcotest.(check int)
                "equal ptrs hash equally"
                (Syntax.Ptr.hash a)
                (Syntax.Ptr.hash b))
         ps)
    ps;
  (* [Hashtbl] over ptrs is the intended use, so check the keys stay apart. *)
  let tbl = Hashtbl.create 16 in
  Array.iteri (fun i p -> Hashtbl.replace tbl (Syntax.Ptr.hash p, i) p) ps;
  Alcotest.(check int) "seven distinct nodes" 7 (Array.length ps);
  Alcotest.(check int)
    "no two ptrs are equal"
    7
    (Array.length (Array.of_list (List.sort_uniq Syntax.Ptr.compare (Array.to_list ps))))
;;

(* [elem] accessors, the token-cursor accessors reached through them, and the
   two printers. *)
let test_elem_and_token_accessors () =
  let r = root_cursor (Cache.create ()) in
  let let0 = (nodes r).(1) in
  let cs = Syntax.children_array let0 in
  if Array.length cs <> 2 then Alcotest.fail "expected LET to be [KW; BODY]";
  let kw_e = cs.(0)
  and body_e = cs.(1) in
  match kw_e, body_e with
  | Syntax.Token kw, Syntax.Node body ->
    Alcotest.(check int) "elem_kind on a token" K.kw (Syntax.elem_kind kw_e);
    Alcotest.(check int) "elem_kind on a node" K.body (Syntax.elem_kind body_e);
    Alcotest.(check (pair int int))
      "elem_text_range on a token"
      (0, 4)
      (Syntax.elem_text_range kw_e);
    Alcotest.(check (pair int int))
      "elem_text_range on a node"
      (4, 9)
      (Syntax.elem_text_range body_e);
    Alcotest.(check int) "token index_in_parent" 0 (Syntax.Token.index_in_parent kw);
    Alcotest.(check int) "node index_in_parent" 1 (Syntax.index_in_parent body);
    Alcotest.(check string)
      "Token.green reaches the underlying token"
      "let "
      (Green.Token.text (Syntax.Token.green kw));
    (* of_elem must agree with the arm-specific constructors. *)
    Alcotest.(check bool)
      "of_elem = of_token on a token"
      true
      (Syntax.Ptr.equal (Syntax.Ptr.of_elem kw_e) (Syntax.Ptr.of_token kw));
    Alcotest.(check bool)
      "of_elem = of_node on a node"
      true
      (Syntax.Ptr.equal (Syntax.Ptr.of_elem body_e) (Syntax.Ptr.of_node body))
  | Syntax.Node _, _ | _, Syntax.Token _ -> Alcotest.fail "expected LET to be [KW; BODY]"
;;

let test_printers () =
  let r = root_cursor (Cache.create ()) in
  let inner0 = (nodes r).(3) in
  (* Syntax.pp delegates to Green.pp, so the two must agree. *)
  Alcotest.(check string)
    "Syntax.pp matches Green.pp"
    (Format.asprintf "%a" Green.pp (Syntax.green inner0))
    (Format.asprintf "%a" Syntax.pp inner0);
  (* ROOT -> LET[0] -> BODY[1] -> INNER[1], then the kind. *)
  Alcotest.(check string)
    "Ptr.pp renders path then kind"
    "/0/1/1:K40"
    (Format.asprintf "%a" Syntax.Ptr.pp (Syntax.Ptr.of_node inner0));
  Alcotest.(check string)
    "Ptr.pp at the root"
    "/:K100"
    (Format.asprintf "%a" Syntax.Ptr.pp (Syntax.Ptr.of_node r))
;;

(* -- offset lookup --------------------------------------------------------- *)

let test_token_at_offset_exhaustive () =
  let r = root_cursor (Cache.create ()) in
  let src = Syntax.to_source r in
  let len = String.length src in
  Alcotest.(check int) "fixture source length" 18 len;
  for off = 0 to len - 1 do
    match Syntax.token_at_offset r off with
    | None -> Alcotest.failf "no token at in-range offset %d" off
    | Some tc ->
      let lo, hi = Syntax.Token.text_range tc in
      Alcotest.(check bool)
        (Printf.sprintf "offset %d inside [%d,%d)" off lo hi)
        true
        (off >= lo && off < hi);
      (* The token's own text must match the source at that span. *)
      Alcotest.(check string)
        (Printf.sprintf "text at offset %d" off)
        (String.sub src lo (hi - lo))
        (Syntax.Token.text tc)
  done
;;

let test_offset_boundaries () =
  let r = root_cursor (Cache.create ()) in
  let text off =
    match Syntax.token_at_offset r off with
    | Some tc -> Some (Syntax.Token.text tc)
    | None -> None
  in
  Alcotest.(check (option string)) "offset 0" (Some "let ") (text 0);
  Alcotest.(check (option string)) "offset 3 (last of KW)" (Some "let ") (text 3);
  (* Half-open: the boundary belongs to the right-hand token. *)
  Alcotest.(check (option string)) "offset 4 (boundary)" (Some "foo") (text 4);
  Alcotest.(check (option string)) "offset 17 (last char)" (Some "xy") (text 17);
  Alcotest.(check (option string)) "offset 18 (one past end)" None (text 18);
  Alcotest.(check (option string)) "offset 99 (far past end)" None (text 99);
  Alcotest.(check bool) "negative offset" true (Syntax.token_at_offset r (-1) = None)
;;

let test_node_at_offset_is_innermost () =
  let r = root_cursor (Cache.create ()) in
  let kind_at off =
    match Syntax.node_at_offset r off with
    | Some n -> Some (Syntax.kind n)
    | None -> None
  in
  (* offset 0 is inside KW, whose parent is LET *)
  Alcotest.(check (option int)) "offset 0 → LET" (Some K.let_) (kind_at 0);
  (* offset 5 is inside ID "foo", whose parent is BODY *)
  Alcotest.(check (option int)) "offset 5 → BODY" (Some K.body) (kind_at 5);
  (* offset 7 is inside INNER's token *)
  Alcotest.(check (option int)) "offset 7 → INNER" (Some K.inner) (kind_at 7);
  Alcotest.(check (option int)) "offset 18 → None" None (kind_at 18)
;;

(* The hover path end to end: byte offset → token → enclosing subject. *)
let test_hover_walk_to_subject () =
  let r = root_cursor (Cache.create ()) in
  let subject_at off =
    Option.bind (Syntax.token_at_offset r off) (fun tc ->
      Syntax.Token.parent tc
      |> Syntax.ancestors
      |> Seq.find (fun n -> Syntax.kind n = K.let_))
  in
  match subject_at 17 with
  | None -> Alcotest.fail "offset 17 should sit inside a LET"
  | Some n ->
    let lo, hi = Syntax.text_range n in
    Alcotest.(check (pair int int)) "second definition" (9, 18) (lo, hi);
    (* And the subject can be addressed for a cache key. *)
    Alcotest.(check int)
      "ptr depth 1 (top-level item)"
      1
      (Syntax.Ptr.depth (Syntax.Ptr.of_node n))
;;

(* -- ancestors / traversal ------------------------------------------------- *)

let test_ancestors_includes_self () =
  let r = root_cursor (Cache.create ()) in
  let inner0 = (nodes r).(3) in
  let ks = Syntax.ancestors inner0 |> Seq.map Syntax.kind |> List.of_seq in
  Alcotest.(check (list int))
    "self, then up to the root"
    [ K.inner; K.body; K.let_; K.root ]
    ks
;;

let test_descendants_order () =
  let r = root_cursor (Cache.create ()) in
  let ks = Syntax.descendants r |> Seq.map Syntax.kind |> List.of_seq in
  Alcotest.(check (list int))
    "preorder, source order"
    [ K.root; K.let_; K.body; K.inner; K.let_; K.body; K.inner ]
    ks
;;

let test_preorder_skip_prunes () =
  let r = root_cursor (Cache.create ()) in
  let visited = ref [] in
  Syntax.preorder r ~f:(fun n ->
    visited := Syntax.kind n :: !visited;
    if Syntax.kind n = K.body then Syntax.Skip else Syntax.Descend);
  Alcotest.(check (list int))
    "BODY visited but its subtree pruned"
    [ K.root; K.let_; K.body; K.let_; K.body ]
    (List.rev !visited)
;;

(* -- deep trees ------------------------------------------------------------ *)

let deep_chain cache depth =
  let rec build i acc =
    if i = 0 then acc else build (i - 1) (mk_node cache 7 [| Green.Node acc |])
  in
  build depth (mk_node cache 8 [| Green.Token (mk_tok cache K.id "x") |])
;;

let test_deep_no_overflow () =
  let cache = Cache.create () in
  let depth = 10_000 in
  let r = Syntax.of_root (deep_chain cache depth) in
  (* traversal *)
  let count = ref 0 in
  Syntax.preorder r ~f:(fun _ ->
    incr count;
    Syntax.Descend);
  Alcotest.(check int) "preorder visits every level" (depth + 1) !count;
  Alcotest.(check int)
    "descendants agrees"
    (depth + 1)
    (Seq.fold_left (fun a _ -> a + 1) 0 (Syntax.descendants r));
  (* descent *)
  (match Syntax.token_at_offset r 0 with
   | Some tc -> Alcotest.(check string) "deep token found" "x" (Syntax.Token.text tc)
   | None -> Alcotest.fail "should find the token at the bottom");
  (* ptr round-trip at depth *)
  let leaf = Seq.fold_left (fun _ n -> n) r (Syntax.descendants r) in
  let p = Syntax.Ptr.of_node leaf in
  Alcotest.(check int) "leaf ptr depth" depth (Syntax.Ptr.depth p);
  match Syntax.Ptr.resolve_node r p with
  | Some l -> same "deep ptr round-trips" l leaf
  | None -> Alcotest.fail "deep ptr should resolve"
;;

(* -- properties ------------------------------------------------------------ *)

module Gen = QCheck2.Gen

type shape =
  | Tok of string
  | Nd of int * shape list

let rec gen_shape depth =
  if depth <= 0
  then
    Gen.map
      (fun s -> Tok s)
      (Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 3))
  else
    Gen.oneof_weighted
      [ ( 2
        , Gen.map
            (fun s -> Tok s)
            (Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 3)) )
      ; ( 3
        , Gen.map2
            (fun k cs -> Nd (k, cs))
            (Gen.int_range 50 55)
            (Gen.list_size (Gen.int_range 1 4) (gen_shape (depth - 1))) )
      ]
;;

(* Always a node at the root, so [of_root] has something to hold. *)
let gen_tree =
  Gen.map2
    (fun k cs -> Nd (k, cs))
    (Gen.int_range 50 55)
    (Gen.list_size (Gen.int_range 1 4) (gen_shape 3))
;;

let rec build cache = function
  | Tok s -> Green.Token (mk_tok cache 9 s)
  | Nd (k, cs) -> Green.Node (mk_node cache k (Array.of_list (List.map (build cache) cs)))
;;

let build_root cache sh =
  match build cache sh with
  | Green.Node n -> n
  | Green.Token _ -> assert false
;;

let prop_ptr_roundtrip =
  QCheck2.Test.make
    ~count:300
    ~name:"every node ptr round-trips to the same cursor"
    gen_tree
    (fun sh ->
       let r = Syntax.of_root (build_root (Cache.create ()) sh) in
       Syntax.descendants r
       |> Seq.for_all (fun n ->
         match Syntax.Ptr.resolve_node r (Syntax.Ptr.of_node n) with
         | Some n' -> n' == n
         | None -> false))
;;

let prop_token_at_offset_partitions =
  QCheck2.Test.make
    ~count:300
    ~name:"token_at_offset agrees with text_range at every offset"
    gen_tree
    (fun sh ->
       let r = Syntax.of_root (build_root (Cache.create ()) sh) in
       let src = Syntax.to_source r in
       let ok = ref true in
       String.iteri
         (fun off _ ->
            match Syntax.token_at_offset r off with
            | None -> ok := false
            | Some tc ->
              let lo, hi = Syntax.Token.text_range tc in
              if
                not
                  (off >= lo
                   && off < hi
                   && String.sub src lo (hi - lo) = Syntax.Token.text tc)
              then ok := false)
         src;
       (* and nothing at or past the end *)
       !ok && Syntax.token_at_offset r (String.length src) = None)
;;

let prop_ancestors_matches_ptr_prefix =
  QCheck2.Test.make
    ~count:200
    ~name:"is_ancestor agrees with the parent chain"
    gen_tree
    (fun sh ->
       let r = Syntax.of_root (build_root (Cache.create ()) sh) in
       Syntax.descendants r
       |> Seq.for_all (fun n ->
         let pn = Syntax.Ptr.of_node n in
         (* every strict ancestor of [n] must test as such, and [n] must not *)
         let strict = Syntax.ancestors n |> Seq.drop 1 in
         Seq.for_all (fun a -> Syntax.Ptr.is_ancestor (Syntax.Ptr.of_node a) pn) strict
         && not (Syntax.Ptr.is_ancestor pn pn)))
;;

let () =
  Alcotest.run
    "nav"
    [ ( "ptr"
      , [ Alcotest.test_case
            "distinguishes occurrences of one green node"
            `Quick
            test_ptr_distinguishes_occurrences
        ; Alcotest.test_case "node round-trip" `Quick test_ptr_roundtrip_nodes
        ; Alcotest.test_case "token round-trip" `Quick test_ptr_roundtrip_tokens
        ; Alcotest.test_case "root ptr" `Quick test_ptr_root
        ; Alcotest.test_case "resolve is total" `Quick test_ptr_resolve_is_total
        ; Alcotest.test_case "is_ancestor" `Quick test_ptr_is_ancestor
        ; Alcotest.test_case
            "hash agrees with equal"
            `Quick
            test_ptr_hash_agrees_with_equal
        ; Alcotest.test_case
            "elem and token accessors"
            `Quick
            test_elem_and_token_accessors
        ; Alcotest.test_case "printers" `Quick test_printers
        ] )
    ; ( "offset lookup"
      , [ Alcotest.test_case
            "every offset finds its token"
            `Quick
            test_token_at_offset_exhaustive
        ; Alcotest.test_case "boundaries are half-open" `Quick test_offset_boundaries
        ; Alcotest.test_case
            "node_at_offset is innermost"
            `Quick
            test_node_at_offset_is_innermost
        ; Alcotest.test_case "hover walk to subject" `Quick test_hover_walk_to_subject
        ] )
    ; ( "traversal"
      , [ Alcotest.test_case "ancestors includes self" `Quick test_ancestors_includes_self
        ; Alcotest.test_case "descendants order" `Quick test_descendants_order
        ; Alcotest.test_case "preorder Skip prunes" `Quick test_preorder_skip_prunes
        ; Alcotest.test_case "10 000 levels, no overflow" `Quick test_deep_no_overflow
        ] )
    ; ( "properties"
      , [ qcheck prop_ptr_roundtrip
        ; qcheck prop_token_at_offset_partitions
        ; qcheck prop_ancestors_matches_ptr_prefix
        ] )
    ]
;;
