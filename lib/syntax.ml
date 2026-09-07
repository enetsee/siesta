(* Red (cursor) layer.

   A node cursor pairs a green pointer with position information: parent,
   offset, index_in_parent. [children_mem] is mutable for memoization only. It
   sits at [None] until first navigated, then holds the materialized array for
   good. *)

type t =
  { green : Green.node
  ; parent : t option
  ; offset : int
  ; index_in_parent : int
  ; mutable children_mem : elem array option
  }

and token_cursor =
  { tc_green : Green.token
  ; tc_parent : t
  ; tc_offset : int
  ; tc_index_in_parent : int
  }

and elem =
  | Node of t
  | Token of token_cursor

type 'a result =
  { root : 'a
  ; self : 'a
  }

let of_root g =
  { green = g; parent = None; offset = 0; index_in_parent = 0; children_mem = None }
;;

let kind t = Green.kind t.green
let green t = t.green
let parent t = t.parent
let index_in_parent t = t.index_in_parent
let text_range t = t.offset, t.offset + Green.text_len t.green

let rec root_of t =
  match t.parent with
  | None -> t
  | Some p -> root_of p
;;

let same_tree a b = root_of a == root_of b
let equal a b = a.offset = b.offset && Green.equal a.green b.green

(* Build the children array on demand. Every later call gets the same array, so
   navigation is stable. One left to right sweep does it, since each child's
   offset is the running sum of the text lengths before it; the offsets stay in
   a ref and each child is read from the green node once. [Array.init] leaves
   its evaluation order unspecified and the running offset depends on one, hence
   the explicit loop.

   This memo is why a cursor tree belongs to one domain. Two domains navigating
   the same tree would each build an array and one write would win, so the
   [parent <-> child] round-trip stops holding. No lock here: [of_root] is O(1),
   so a second domain gets its own cursor tree over the same green root for the
   price of a record. *)
let materialize_children parent_cursor =
  let g = parent_cursor.green in
  let n = Green.num_children g in
  if n = 0
  then [||]
  else (
    let off = ref parent_cursor.offset in
    let elem_of i =
      match Green.nth_child g i with
      | None -> assert false
      | Some c ->
        let child_off = !off in
        off := child_off + Green.child_text_len c;
        (match c with
         | Green.Node cg ->
           Node
             { green = cg
             ; parent = Some parent_cursor
             ; offset = child_off
             ; index_in_parent = i
             ; children_mem = None
             }
         | Green.Token t ->
           Token
             { tc_green = t
             ; tc_parent = parent_cursor
             ; tc_offset = child_off
             ; tc_index_in_parent = i
             })
    in
    (* Seeded with child 0 to give [Array.make] an element; the loop then runs
       strictly left to right, as [off] requires. *)
    let out = Array.make n (elem_of 0) in
    for i = 1 to n - 1 do
      out.(i) <- elem_of i
    done;
    out)
;;

let ensure_children t =
  match t.children_mem with
  | Some c -> c
  | None ->
    let c = materialize_children t in
    t.children_mem <- Some c;
    c
;;

(* Copy. Without it the caller holds the memo itself, and reordering or editing
   it leaves [nth_child] and [preorder] disagreeing with the green child order,
   and each cursor's [index_in_parent] disagreeing with its own slot, for the
   life of the cursor. The elements are shared, so the cursor identities 
   [nth_child] hands back are unaffected. *)
let children_array t = Array.copy (ensure_children t)

let nth_child t i =
  let cs = ensure_children t in
  if i < 0 || i >= Array.length cs then None else Some cs.(i)
;;

let to_source t = Green.to_source t.green
let pp ppf t = Green.pp ppf t.green

(* -- token cursor accessors ------------------------------------------------ *)

module Token = struct
  let kind tc = Green.Token.kind tc.tc_green
  let text tc = Green.Token.text tc.tc_green
  let green tc = tc.tc_green
  let parent tc = tc.tc_parent
  let index_in_parent tc = tc.tc_index_in_parent

  let text_range tc =
    tc.tc_offset, tc.tc_offset + String.length (Green.Token.text tc.tc_green)
  ;;

  let equal a b = a.tc_offset = b.tc_offset && Green.Token.equal a.tc_green b.tc_green
end

(* -- elem accessors -------------------------------------------------------- *)

let elem_kind = function
  | Node t -> Green.kind t.green
  | Token tc -> Green.Token.kind tc.tc_green
;;

let elem_text_range = function
  | Node t -> t.offset, t.offset + Green.text_len t.green
  | Token tc -> Token.text_range tc
;;

(* -- upward navigation ----------------------------------------------------- *)

(* Self first, then each parent in turn. The [Seq] holds only the current
   cursor, so stopping early costs only the steps taken. *)
let ancestors t =
  let rec step cur () =
    match cur with
    | None -> Seq.Nil
    | Some c -> Seq.Cons (c, step c.parent)
  in
  step (Some t)
;;

(* -- offset lookup --------------------------------------------------------- *)

(* [elem_text_range] returns both ends as a tuple. [child_containing] compares
   one end at a time, so it reads them separately and allocates nothing. *)
let[@inline] elem_lo = function
  | Node t -> t.offset
  | Token tc -> tc.tc_offset
;;

let[@inline] elem_hi = function
  | Node t -> t.offset + Green.text_len t.green
  | Token tc -> tc.tc_offset + String.length (Green.Token.text tc.tc_green)
;;

(* The child of [cur] whose half-open range contains [offset]. Children are
   contiguous and in source order, so ends are non-decreasing and the child
   wanted is the first whose end is past [offset]; binary search for it. That
   child can still start after [offset], but only when [offset] is left of [cur]
   itself, which is what the [elem_lo] check catches. Zero-width children have
   [hi = lo], so no offset is past their end and the search skips them. *)
let child_containing cur offset =
  let cs = ensure_children cur in
  let n = Array.length cs in
  let lo = ref 0 in
  let hi = ref n in
  while !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    if elem_hi cs.(mid) > offset then hi := mid else lo := mid + 1
  done;
  if !lo >= n
  then None
  else (
    let c = cs.(!lo) in
    if elem_lo c <= offset then Some c else None)
;;

let in_range t offset =
  let lo, hi = text_range t in
  offset >= lo && offset < hi
;;

(* Both descents recurse in tail position, so depth costs no stack. *)
let rec descend_to_token cur offset =
  match child_containing cur offset with
  | None -> None
  | Some (Token tc) -> Some tc
  | Some (Node c) -> descend_to_token c offset
;;

let token_at_offset root offset =
  if in_range root offset then descend_to_token root offset else None
;;

let rec descend_to_node cur offset =
  match child_containing cur offset with
  | Some (Node c) -> descend_to_node c offset
  | None | Some (Token _) -> cur
;;

let node_at_offset root offset =
  if in_range root offset then Some (descend_to_node root offset) else None
;;

(* -- downward traversal ---------------------------------------------------- *)

type visit =
  | Descend
  | Skip

(* Children go on right to left so they pop in source order. The explicit stack
   keeps the walk safe on arbitrarily deep trees. *)
let push_children stack cur =
  let cs = ensure_children cur in
  for i = Array.length cs - 1 downto 0 do
    match cs.(i) with
    | Node c -> stack := c :: !stack
    | Token _ -> ()
  done
;;

let preorder t ~f =
  let stack = ref [ t ] in
  let continue_ = ref true in
  while !continue_ do
    match !stack with
    | [] -> continue_ := false
    | cur :: rest ->
      stack := rest;
      (match f cur with
       | Skip -> ()
       | Descend -> push_children stack cur)
  done
;;

let descendants t =
  let rec step stack () =
    match stack with
    | [] -> Seq.Nil
    | cur :: rest ->
      let s = ref rest in
      push_children s cur;
      Seq.Cons (cur, step !s)
  in
  step [ t ]
;;

(* -- pointers -------------------------------------------------------------- *)

(* Indices from root down to [t], inclusive of [t.index_in_parent]. *)
let path_from_root t =
  let rec loop acc cur =
    match cur.parent with
    | None -> acc
    | Some p -> loop (cur.index_in_parent :: acc) p
  in
  loop [] t
;;

module Ptr = struct
  (* Bind the outer node cursor before [t] below shadows it. *)
  type cursor = t

  type nonrec t =
    { path : int array
    ; kind : int
    }

  let of_node (n : cursor) =
    { path = Array.of_list (path_from_root n); kind = Green.kind n.green }
  ;;

  let of_token tc =
    { path = Array.of_list (path_from_root tc.tc_parent @ [ tc.tc_index_in_parent ])
    ; kind = Green.Token.kind tc.tc_green
    }
  ;;

  let of_elem = function
    | Node n -> of_node n
    | Token tc -> of_token tc
  ;;

  let kind (p : t) = p.kind
  let depth (p : t) = Array.length p.path

  (* The kind check at the target is what makes a stale ptr fail cleanly. *)
  let resolve (root : cursor) (p : t) =
    let n = Array.length p.path in
    if n = 0
    then if Green.kind root.green = p.kind then Some (Node root) else None
    else (
      let rec loop cur i =
        let cs = ensure_children cur in
        let idx = p.path.(i) in
        if idx < 0 || idx >= Array.length cs
        then None
        else if i = n - 1
        then (
          let e = cs.(idx) in
          if elem_kind e = p.kind then Some e else None)
        else (
          match cs.(idx) with
          | Node c -> loop c (i + 1)
          | Token _ -> None)
      in
      loop root 0)
  ;;

  let resolve_node (root : cursor) (p : t) =
    match resolve root p with
    | Some (Node n) -> Some n
    | Some (Token _) | None -> None
  ;;

  let equal (a : t) (b : t) =
    let n = Array.length a.path in
    a.kind = b.kind
    && n = Array.length b.path
    &&
    let rec go i = i >= n || (a.path.(i) = b.path.(i) && go (i + 1)) in
    go 0
  ;;

  (* Lexicographic on the path, so a ptr sorts immediately before its
     descendants; kind breaks ties. *)
  let compare (a : t) (b : t) =
    let la = Array.length a.path
    and lb = Array.length b.path in
    let rec go i =
      if i >= la && i >= lb
      then Int.compare a.kind b.kind
      else if i >= la
      then -1
      else if i >= lb
      then 1
      else (
        let c = Int.compare a.path.(i) b.path.(i) in
        if c <> 0 then c else go (i + 1))
    in
    go 0
  ;;

  (* The same mixer [Dedup] uses. A path is a run of small, near-consecutive
     child indices, which needs the multiply and shift to scatter. *)
  let[@inline] mix h x =
    let h = h lxor x * 0x9E3779B1 in
    h lxor (h lsr 29)
  ;;

  let hash (p : t) =
    let h = ref (mix (Array.length p.path) p.kind) in
    Array.iter (fun i -> h := mix !h i) p.path;
    let h = !h in
    let h = h lxor (h lsr 30) * 0x3F58476D1CE4E5B9 in
    let h = h lxor (h lsr 27) * 0x14D049BB133111EB in
    h lxor (h lsr 31) land max_int
  ;;

  (* Proper prefix: [a] addresses a strict ancestor of [b]. *)
  let is_ancestor (a : t) (b : t) =
    let la = Array.length a.path
    and lb = Array.length b.path in
    la < lb
    &&
    let rec go i = i >= la || (a.path.(i) = b.path.(i) && go (i + 1)) in
    go 0
  ;;

  let pp ppf (p : t) =
    Format.fprintf ppf "@[<h>/";
    Array.iteri
      (fun i idx ->
         if i = 0 then Format.fprintf ppf "%d" idx else Format.fprintf ppf "/%d" idx)
      p.path;
    Format.fprintf ppf ":K%d@]" p.kind
  ;;
end

(* -- mutation -------------------------------------------------------------- *)

(* Walk a fresh root cursor down a path of child indices to the leaf. *)
let walk_path root path =
  let rec loop cur = function
    | [] -> cur
    | i :: rest ->
      let cs = ensure_children cur in
      (match cs.(i) with
       | Node child -> loop child rest
       | Token _ -> failwith "Syntax: path index points at a token, not a node")
  in
  loop root path
;;

(* Walk up from the target, substituting the changed child slot at each level.
   Hash-consing leaves everything off the spine physically shared with the old
   tree. *)
let rec rebuild_spine cache cur new_self_green =
  match cur.parent with
  | None -> new_self_green
  | Some p ->
    let new_cs = Green.children_array p.green in
    new_cs.(cur.index_in_parent) <- Green.Node new_self_green;
    let new_p_green =
      Green.mk_node
        cache
        ~kind:(Green.kind p.green)
        ~payload:(Green.payload p.green)
        ~children:new_cs
        ()
    in
    rebuild_spine cache p new_p_green
;;

let replace cache target new_green =
  let new_root_green = rebuild_spine cache target new_green in
  let new_root = of_root new_root_green in
  let path = path_from_root target in
  let new_self = walk_path new_root path in
  { root = new_root; self = new_self }
;;

let splice_children cache target ~at ~remove inserts =
  let g = target.green in
  let n = Green.num_children g in
  (* Validated ahead of the allocations, so a rejected call pays for the two
     comparisons alone. *)
  if at < 0 || at > n
  then
    invalid_arg (Printf.sprintf "Syntax.splice_children: at=%d out of range [0..%d]" at n);
  if remove < 0 || at + remove > n
  then
    invalid_arg
      (Printf.sprintf
         "Syntax.splice_children: remove=%d at=%d exceeds children=%d"
         remove
         at
         n);
  let inserts_arr = Array.of_list inserts in
  let ins = Array.length inserts_arr in
  let child j =
    match Green.nth_child g j with
    | Some c -> c
    | None -> assert false
  in
  (* Read straight into the result. [Green.children_array] would copy every
     child into a private array that is then only read, and the kept ends would
     be copied again coming out of it. *)
  let new_cs =
    Array.init
      (n - remove + ins)
      (fun j ->
         if j < at
         then child j
         else if j < at + ins
         then inserts_arr.(j - at)
         else child (j - ins + remove))
  in
  let new_target_green =
    Green.mk_node
      cache
      ~kind:(Green.kind target.green)
      ~payload:(Green.payload target.green)
      ~children:new_cs
      ()
  in
  replace cache target new_target_green
;;

(* The (parent, index) pair an elem's own back-pointers give you. A node cursor
   at the root has no parent, so that is a caller mistake and fails loudly. *)
let parent_index_of_elem = function
  | Node t ->
    (match t.parent with
     | Some p -> p, t.index_in_parent
     | None -> failwith "Syntax: cursor is at root, use [replace] for root rewrites")
  | Token tc -> tc.tc_parent, tc.tc_index_in_parent
;;

let replace_child cache child new_green =
  let parent, idx = parent_index_of_elem child in
  splice_children cache parent ~at:idx ~remove:1 [ new_green ]
;;

let splice_at cache child ~remove inserts =
  let parent, idx = parent_index_of_elem child in
  splice_children cache parent ~at:idx ~remove inserts
;;
