(* A toy calculator language built on `siesta`.

   - Typed lexer with its own token variant, translated to siesta's [int] kinds
     at the lexer/builder boundary and nowhere else.
   - Recursive-descent parser driving [Builder], with checkpoints.
   - Typed AST views over the red layer.
   - Lowering from those views to a semantic AST [Ir.t], whose [Hole]
     constructor stands in for a syntax error, evaluated by [eval_ir].
   - Constant folding as a bottom-up walk over green nodes.
   - An id map from literal ordinals to [Ptr]s, rebuilt on each parse.

   Run: `dune exec examples/calc/calc.exe`. *)

open Siesta

(* ---- typed lexer tokens -------------------------------------------------- *)

type tok =
  | T_int of string (* "42" *)
  | T_plus
  | T_minus
  | T_star
  | T_slash
  | T_lparen
  | T_rparen
  | T_ws of string (* preserved for lossless reconstruction *)
  | T_bad of string (* unrecognized characters *)

(* ---- siesta kinds -------------------------------------------------------- *)

(* The library wants ints. The lexer-side representation stays typed and the
   translation happens in exactly one place, [emit] below. *)
module K = struct
  (* tokens *)
  let int_lit = 1
  let plus = 2
  let minus = 3
  let star = 4
  let slash = 5
  let lparen = 6
  let rparen = 7
  let ws = 8
  let bad = 9

  (* nodes *)
  let int_node = 20
  let bin_expr = 21
  let paren_expr = 22
  let error_node = 99
  let root = 100
end

(* The single point of translation between the lexer's typed view and siesta's
   (kind, text) pairs. *)
let to_pic = function
  | T_int s -> K.int_lit, s
  | T_plus -> K.plus, "+"
  | T_minus -> K.minus, "-"
  | T_star -> K.star, "*"
  | T_slash -> K.slash, "/"
  | T_lparen -> K.lparen, "("
  | T_rparen -> K.rparen, ")"
  | T_ws s -> K.ws, s
  | T_bad s -> K.bad, s
;;

(* ---- lexer --------------------------------------------------------------- *)

let lex src =
  let len = String.length src in
  let out = ref [] in
  let i = ref 0 in
  let slice start = String.sub src start (!i - start) in
  while !i < len do
    let c = src.[!i] in
    let start = !i in
    if c = ' ' || c = '\t' || c = '\n'
    then (
      while
        !i < len
        &&
        let c = src.[!i] in
        c = ' ' || c = '\t' || c = '\n'
      do
        incr i
      done;
      out := T_ws (slice start) :: !out)
    else if c >= '0' && c <= '9'
    then (
      while
        !i < len
        &&
        let c = src.[!i] in
        c >= '0' && c <= '9'
      do
        incr i
      done;
      out := T_int (slice start) :: !out)
    else (
      let t =
        match c with
        | '+' -> T_plus
        | '-' -> T_minus
        | '*' -> T_star
        | '/' -> T_slash
        | '(' -> T_lparen
        | ')' -> T_rparen
        | _ -> T_bad (String.make 1 c)
      in
      incr i;
      out := t :: !out)
  done;
  List.rev !out
;;

(* ---- parser -------------------------------------------------------------- *)

(* Grammar, left-associative:

     expr := term (('+' | '-') term)*
     term := atom (('*' | '/') atom)*
     atom := T_int | '(' expr ')'

   [Builder.checkpoint] and [start_node_at] wrap the LHS retroactively, once an
   operator has been seen. *)

type ps =
  { mutable toks : tok list
  ; builder : Builder.t
  }

(* The parser never calls [Builder.token] itself. This is the one place that
   crosses the lexer/siesta boundary. *)
let emit p t =
  let kind, text = to_pic t in
  Builder.token p.builder kind text
;;

let consume p =
  match p.toks with
  | t :: rest ->
    emit p t;
    p.toks <- rest
  | [] -> ()
;;

(* Spelled out in full, so adding a token kind breaks these three and makes you
   say where it belongs. *)
let is_ws = function
  | T_ws _ -> true
  | T_int _ | T_plus | T_minus | T_star | T_slash | T_lparen | T_rparen | T_bad _ -> false
;;

let is_addop = function
  | T_plus | T_minus -> true
  | T_int _ | T_star | T_slash | T_lparen | T_rparen | T_ws _ | T_bad _ -> false
;;

let is_mulop = function
  | T_star | T_slash -> true
  | T_int _ | T_plus | T_minus | T_lparen | T_rparen | T_ws _ | T_bad _ -> false
;;

let consume_ws p =
  let rec loop () =
    match p.toks with
    | t :: _ when is_ws t ->
      consume p;
      loop ()
    | _ :: _ | [] -> ()
  in
  loop ()
;;

let peek p =
  let rec skip = function
    | t :: rest when is_ws t -> skip rest
    | rest -> rest
  in
  match skip p.toks with
  | t :: _ -> Some t
  | [] -> None
;;

let rec parse_expr p = parse_left_assoc p ~next:parse_term ~op:is_addop
and parse_term p = parse_left_assoc p ~next:parse_atom ~op:is_mulop

and parse_left_assoc p ~next ~op =
  let cp = Builder.checkpoint p.builder in
  next p;
  consume_ws p;
  let rec loop () =
    match peek p with
    | Some t when op t ->
      Builder.start_node_at p.builder cp K.bin_expr;
      consume_ws p;
      consume p (* the operator *);
      consume_ws p;
      next p;
      Builder.finish_node p.builder;
      consume_ws p;
      loop ()
    | Some _ | None -> ()
  in
  loop ()

and parse_atom p =
  consume_ws p;
  match peek p with
  | Some (T_int _) ->
    Builder.start_node p.builder K.int_node;
    consume p;
    Builder.finish_node p.builder
  | Some T_lparen ->
    Builder.start_node p.builder K.paren_expr;
    consume p;
    parse_expr p;
    consume_ws p;
    (match peek p with
     | Some T_rparen -> consume p
     | Some (T_int _ | T_plus | T_minus | T_star | T_slash | T_lparen | T_ws _ | T_bad _)
     | None ->
       Builder.start_node p.builder K.error_node;
       Builder.finish_node p.builder);
    Builder.finish_node p.builder
  | Some (T_plus | T_minus | T_star | T_slash | T_rparen | T_ws _ | T_bad _) | None ->
    Builder.start_node p.builder K.error_node;
    (match p.toks with
     | _ :: _ -> consume p
     | [] -> ());
    Builder.finish_node p.builder
;;

(* The cache comes back with the root because every edit below ([constant_fold],
   [set_literal], [parenthesise] and the rest) interns through the one the tree
   was built with, keeping the rebuilt spine sharing with the tree it came from.
   [Builder.create] allocates one when [?cache] is omitted and [Builder.cache]
   hands that one back, so [?cache] passes straight through. *)
let parse ?cache src =
  let builder = Builder.create ?cache () in
  Builder.start_node builder K.root;
  let p = { toks = lex src; builder } in
  parse_expr p;
  consume_ws p;
  while p.toks <> [] do
    Builder.start_node builder K.error_node;
    consume p;
    Builder.finish_node builder
  done;
  Builder.finish_node builder;
  Builder.cache builder, Builder.finish builder
;;

(* ---- typed AST views ------------------------------------------------------ *)

module Ast = struct
  let child_nodes n =
    Syntax.children_array n
    |> Array.to_list
    |> List.filter_map (function
      | Syntax.Node x -> Some x
      | Syntax.Token _ -> None)
  ;;

  let child_tokens n =
    Syntax.children_array n
    |> Array.to_list
    |> List.filter_map (function
      | Syntax.Token t -> Some t
      | Syntax.Node _ -> None)
  ;;

  let token_of_kind k n =
    List.find_opt (fun t -> Syntax.Token.kind t = k) (child_tokens n)
  ;;

  module Int_node = struct
    type t = Syntax.t

    let cast n = if Syntax.kind n = K.int_node then Some n else None
    let syntax t = t

    let value t =
      token_of_kind K.int_lit t
      |> Option.map (fun tc -> int_of_string (Syntax.Token.text tc))
    ;;
  end

  module rec Expr : sig
    type t =
      | Int of Int_node.t
      | Bin of Bin_expr.t
      | Paren of Paren_expr.t

    val cast : Syntax.t -> t option
  end = struct
    type t =
      | Int of Int_node.t
      | Bin of Bin_expr.t
      | Paren of Paren_expr.t

    let cast n =
      let k = Syntax.kind n in
      if k = K.int_node
      then Option.map (fun x -> Int x) (Int_node.cast n)
      else if k = K.bin_expr
      then Option.map (fun x -> Bin x) (Bin_expr.cast n)
      else if k = K.paren_expr
      then Option.map (fun x -> Paren x) (Paren_expr.cast n)
      else None
    ;;
  end

  and Bin_expr : sig
    type t

    val cast : Syntax.t -> t option
    val syntax : t -> Syntax.t
    val lhs : t -> Expr.t option
    val rhs : t -> Expr.t option
    val op : t -> Syntax.token_cursor option
  end = struct
    type t = Syntax.t

    let cast n = if Syntax.kind n = K.bin_expr then Some n else None
    let syntax t = t
    let exprs t = List.filter_map Expr.cast (child_nodes t)

    let lhs t =
      match exprs t with
      | x :: _ -> Some x
      | _ -> None
    ;;

    let rhs t =
      match exprs t with
      | _ :: x :: _ -> Some x
      | _ -> None
    ;;

    let is_op_kind k = k = K.plus || k = K.minus || k = K.star || k = K.slash

    let op t =
      List.find_opt (fun tc -> is_op_kind (Syntax.Token.kind tc)) (child_tokens t)
    ;;
  end

  and Paren_expr : sig
    type t

    val cast : Syntax.t -> t option
    val syntax : t -> Syntax.t
    val expr : t -> Expr.t option
  end = struct
    type t = Syntax.t

    let cast n = if Syntax.kind n = K.paren_expr then Some n else None
    let syntax t = t
    let expr t = List.find_map Expr.cast (child_nodes t)
  end

  module Root = struct
    let cast n = if Syntax.kind n = K.root then Some n else None
    let syntax t = t
    let expr t = List.find_map Expr.cast (child_nodes t)
  end
end

(* ---- semantic AST and lowering -------------------------------------------- *)

(* The CST is lossless: it keeps the trivia and carries parse errors as [error]
   nodes in their parent's child list. [Ir.t] is the lossy view later passes
   want, with a single [Hole] constructor covering every syntax error the typed
   views surface as a missing slot.

   Each [Hole] carries the source range of the offending CST node, so a
   diagnostic raised later can still point back at the original text.

   The lowering is total. Every reachable [Ast.Expr.t] becomes some [Ir.t], and
   a [Hole] stands in wherever a view accessor gave [None]. *)

module Ir = struct
  type op =
    | Add
    | Sub
    | Mul
    | Div

  (* Half-open byte-offset span of the source the node was lowered from, the
     same thing [Syntax.text_range] returns. A record, so a diagnostic can name
     the fields and so a file id or ghost flag can be added later without
     rewriting every call site. *)
  type span =
    { start_offset : int
    ; end_offset : int
    }

  type t =
    | Lit of int * span
    | Bin of op * t * t * span
    | Hole of span

  let span_of_range (lo, hi) = { start_offset = lo; end_offset = hi }

  let op_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
  ;;

  let rec to_string = function
    | Lit (n, _) -> string_of_int n
    | Bin (op, a, b, _) ->
      Printf.sprintf "(%s %s %s)" (op_to_string op) (to_string a) (to_string b)
    | Hole s -> Printf.sprintf "<hole@%d..%d>" s.start_offset s.end_offset
  ;;
end

let op_of_token tc =
  let k = Syntax.Token.kind tc in
  if k = K.plus
  then Some Ir.Add
  else if k = K.minus
  then Some Ir.Sub
  else if k = K.star
  then Some Ir.Mul
  else if k = K.slash
  then Some Ir.Div
  else None
;;

let span_of (n : Syntax.t) : Ir.span = Ir.span_of_range (Syntax.text_range n)

let rec lower_expr (e : Ast.Expr.t) : Ir.t =
  match e with
  | Ast.Expr.Int n ->
    let s = span_of (Ast.Int_node.syntax n) in
    (match Ast.Int_node.value n with
     | Some v -> Ir.Lit (v, s)
     | None -> Ir.Hole s)
  | Ast.Expr.Paren n ->
    (match Ast.Paren_expr.expr n with
     | Some inner -> lower_expr inner
     | None -> Ir.Hole (span_of (Ast.Paren_expr.syntax n)))
  | Ast.Expr.Bin n ->
    let s = span_of (Ast.Bin_expr.syntax n) in
    let lower_or_hole = function
      | Some e -> lower_expr e
      | None -> Ir.Hole s
    in
    (match Option.bind (Ast.Bin_expr.op n) op_of_token with
     | None -> Ir.Hole s
     | Some op ->
       Ir.Bin
         (op, lower_or_hole (Ast.Bin_expr.lhs n), lower_or_hole (Ast.Bin_expr.rhs n), s))
;;

let lower_root (root : Green.node) : Ir.t =
  let cursor = Syntax.of_root root in
  match Ast.Root.cast cursor with
  | None -> Ir.Hole (span_of cursor)
  | Some r ->
    (match Ast.Root.expr r with
     | Some e -> lower_expr e
     | None -> Ir.Hole (span_of (Ast.Root.syntax r)))
;;

let rec eval_ir : Ir.t -> int option = function
  | Ir.Lit (v, _) -> Some v
  | Ir.Hole _ -> None
  | Ir.Bin (op, a, b, _) ->
    let ( let* ) = Option.bind in
    let* x = eval_ir a in
    let* y = eval_ir b in
    (match op with
     | Ir.Add -> Some (x + y)
     | Ir.Sub -> Some (x - y)
     | Ir.Mul -> Some (x * y)
     | Ir.Div -> if y = 0 then None else Some (x / y))
;;

(* ---- constant-fold strategy ---------------------------------------------- *)

let int_of_int_node n =
  let cs = Green.children_array n in
  if Array.length cs <> 1
  then None
  else (
    match cs.(0) with
    | Green.Token t when Green.Token.kind t = K.int_lit ->
      Some (int_of_string (Green.Token.text t))
    | Green.Token _ | Green.Node _ -> None)
;;

let mk_int_node cache v =
  let tok = Green.mk_token cache ~kind:K.int_lit ~text:(string_of_int v) in
  Green.mk_node cache ~kind:K.int_node ~children:[| Green.Token tok |] ()
;;

(* Pull (lhs, op, rhs) out of a [BIN_EXPR]'s children, ignoring trivia. [None]
   unless the shape is a clean binary expression over [INT_NODE] operands. *)
let is_ws = function
  | Green.Token t -> Green.Token.kind t = K.ws
  | Green.Node _ -> false
;;

let is_op_kind k = k = K.plus || k = K.minus || k = K.star || k = K.slash

let extract_binop n =
  let cs =
    Green.children_array n |> Array.to_list |> List.filter (fun c -> not (is_ws c))
  in
  match cs with
  | [ x; y; z ] ->
    (match x, y, z with
     | Green.Node a, Green.Token op, Green.Node b
       when Green.kind a = K.int_node
            && Green.kind b = K.int_node
            && is_op_kind (Green.Token.kind op) ->
       (match int_of_int_node a, int_of_int_node b with
        | Some av, Some bv -> Some (av, Green.Token.kind op, bv)
        | None, _ | _, None -> None)
     | (Green.Node _ | Green.Token _), _, _ -> None)
  | [] | [ _ ] | [ _; _ ] | _ :: _ :: _ :: _ :: _ -> None
;;

(* Unwrap a [PAREN_EXPR] around a single [INT_NODE]. Restricted to [INT_NODE]
   deliberately: stripping the parens off a compound inner expression, say
   [(1+x) * y], would quietly change the precedence. *)
let is_paren_or_ws = function
  | Green.Token t ->
    let k = Green.Token.kind t in
    k = K.ws || k = K.lparen || k = K.rparen
  | Green.Node _ -> false
;;

let unwrap_paren_int n =
  if Green.kind n <> K.paren_expr
  then None
  else (
    let cs =
      Green.children_array n
      |> Array.to_list
      |> List.filter (fun c -> not (is_paren_or_ws c))
    in
    match cs with
    | [ x ] ->
      (match x with
       | Green.Node inner when Green.kind inner = K.int_node -> Some inner
       | Green.Node _ | Green.Token _ -> None)
    | [] | _ :: _ :: _ -> None)
;;

let apply_op op a b =
  if op = K.plus
  then Some (a + b)
  else if op = K.minus
  then Some (a - b)
  else if op = K.star
  then Some (a * b)
  else if op = K.slash && b <> 0
  then Some (a / b)
  else None (* division by zero, so leave it alone *)
;;

let fold_binop cache n =
  match extract_binop n with
  | None -> None
  | Some (a, op, b) -> Option.map (mk_int_node cache) (apply_op op a b)
;;

(* Bottom-up rewrite: fold the children first, then the node itself.
   Hash-consing keeps untouched subtrees physically shared with the input, and
   the [any_changed] guard skips the [mk_node] call altogether for a node whose
   children all came back unchanged. *)
let constant_fold cache root =
  let fold n =
    if Green.kind n = K.bin_expr then fold_binop cache n else unwrap_paren_int n
  in
  let rec walk n =
    let cs = Green.children_array n in
    let any_changed = ref false in
    for i = 0 to Array.length cs - 1 do
      match cs.(i) with
      | Green.Token _ -> ()
      | Green.Node child ->
        let child' = walk child in
        if not (Green.equal child child')
        then (
          any_changed := true;
          cs.(i) <- Green.Node child')
    done;
    let n' =
      if !any_changed
      then
        Green.mk_node
          cache
          ~kind:(Green.kind n)
          ~payload:(Green.payload n)
          ~children:cs
          ()
      else n
    in
    match fold n' with
    | Some n'' -> n''
    | None -> n'
  in
  walk root
;;

(* ---- hover ---------------------------------------------------------------- *)

(* What an editor does with a byte offset: find the token under it, then walk up
   to the nearest enclosing expression. *)

let is_expr_kind k = k = K.int_node || k = K.bin_expr || k = K.paren_expr

let hover (root : Green.node) offset =
  let cur = Syntax.of_root root in
  match Syntax.token_at_offset cur offset with
  | None -> None
  | Some tok ->
    let enclosing =
      Syntax.Token.parent tok
      |> Syntax.ancestors
      |> Seq.find (fun n -> is_expr_kind (Syntax.kind n))
    in
    Some (tok, enclosing)
;;

let show_hover root offset =
  match hover root offset with
  | None -> Printf.printf "  hover @%d: nothing there\n" offset
  | Some (tok, enclosing) ->
    let lo, hi = Syntax.Token.text_range tok in
    Printf.printf
      "  hover @%d: token %S at %d..%d, child %d of K%d"
      offset
      (Syntax.Token.text tok)
      lo
      hi
      (Syntax.Token.index_in_parent tok)
      (Syntax.kind (Syntax.Token.parent tok));
    (match enclosing with
     | None -> print_newline ()
     | Some e ->
       Format.printf
         ", inside %S at %a@."
         (Syntax.to_source e)
         Syntax.Ptr.pp
         (Syntax.Ptr.of_node e));
    (* The innermost node covering the offset, and where it sits in its parent.
       [Token.green] drops down to the position-free value underneath. *)
    (match Syntax.node_at_offset (Syntax.of_root root) offset with
     | None -> ()
     | Some n ->
       let parent_kind =
         match Syntax.parent n with
         | Some p -> Printf.sprintf "K%d" (Syntax.kind p)
         | None -> "(root)"
       in
       Printf.printf
         "           innermost K%d, child %d of %s, token tag %d\n"
         (Syntax.kind n)
         (Syntax.index_in_parent n)
         parent_kind
         (Green.Token.tag (Syntax.Token.green tok)))
;;

(* ---- an id map over the literals ------------------------------------------ *)

(* The firewall pattern in miniature. The ids are stable, a literal's ordinal in
   source order, and the ptrs they map to are rebuilt from the tree each time,
   because a ptr is only good against the tree it came from.

   [Skip] is what keeps the walk off the tokens under each literal. *)

let literal_ptrs (root : Green.node) =
  let tbl : (int, Syntax.Ptr.t) Hashtbl.t = Hashtbl.create 16 in
  let n = ref 0 in
  Syntax.preorder (Syntax.of_root root) ~f:(fun cur ->
    if Syntax.kind cur = K.int_node
    then (
      Hashtbl.replace tbl !n (Syntax.Ptr.of_node cur);
      incr n;
      Syntax.Skip)
    else Syntax.Descend);
  tbl
;;

(* ---- edits through the red layer ------------------------------------------ *)

(* Every edit is persistent. The spine from the root down to the target is
   rebuilt, hash-consing hands back the original OCaml value for everything off
   it, and the old tree is untouched and still usable. *)

(* Set literal number [idx] (source order) to [v]. *)
let set_literal cache root idx v =
  let cur = Syntax.of_root root in
  match Hashtbl.find_opt (literal_ptrs root) idx with
  | None -> None
  | Some p ->
    Option.map
      (fun target -> Syntax.replace cache target (mk_int_node cache v))
      (Syntax.Ptr.resolve_node cur p)
;;

(* Swap the operator token of the first BIN_EXPR, through the cursor-based
   helper. *)
let swap_first_operator cache root new_kind new_text =
  let cur = Syntax.of_root root in
  let found = ref None in
  Syntax.preorder cur ~f:(fun n ->
    if Option.is_some !found
    then Syntax.Skip
    else if Syntax.kind n = K.bin_expr
    then (
      Array.iter
        (fun e ->
           if Option.is_none !found && is_op_kind (Syntax.elem_kind e)
           then found := Some e)
        (Syntax.children_array n);
      Syntax.Descend)
    else Syntax.Descend);
  Option.map
    (fun e ->
       let t = Green.mk_token cache ~kind:new_kind ~text:new_text in
       Syntax.replace_child cache e (Green.Token t))
    !found
;;

let non_trivia n =
  Green.children_array n |> Array.to_list |> List.filter (fun c -> not (is_ws c))
;;

(* Wrap the root's expression in parentheses, replacing the root's whole child
   list in one splice. *)
let parenthesise cache root =
  let cur = Syntax.of_root root in
  let n = Green.num_children root in
  match non_trivia root with
  | [ Green.Node expr ] ->
    let lp = Green.mk_token cache ~kind:K.lparen ~text:"(" in
    let rp = Green.mk_token cache ~kind:K.rparen ~text:")" in
    let wrapped =
      Green.mk_node
        cache
        ~kind:K.paren_expr
        ~children:[| Green.Token lp; Green.Node expr; Green.Token rp |]
        ()
    in
    Some (Syntax.splice_children cache cur ~at:0 ~remove:n [ Green.Node wrapped ])
  | [ Green.Token _ ] | [] | _ :: _ :: _ -> None
;;

(* Delete the first whitespace token anywhere in the tree. [splice_at] with an
   empty insert list is how you remove a child you already hold a cursor on. *)
let drop_first_ws cache root =
  let cur = Syntax.of_root root in
  let found = ref None in
  Syntax.preorder cur ~f:(fun n ->
    if Option.is_some !found
    then Syntax.Skip
    else (
      Array.iter
        (fun e ->
           if Option.is_none !found && Syntax.elem_kind e = K.ws then found := Some e)
        (Syntax.children_array n);
      Syntax.Descend));
  Option.map (fun e -> Syntax.splice_at cache e ~remove:1 []) !found
;;

(* ---- demo ----------------------------------------------------------------- *)

let show_shape (root : Green.node) =
  let cur = Syntax.of_root root in
  let nodes = Seq.fold_left (fun a _ -> a + 1) 0 (Syntax.descendants cur) in
  Printf.printf
    "  shape: %d bytes, %d nodes, %d root children\n"
    (Green.text_len root)
    nodes
    (Green.num_children root);
  (* text_len is cached on the node, so it has to match the sum over the
     children. *)
  let cs = Green.children_array root in
  assert (Green.text_len root = Green.sum_text_len cs);
  match Green.nth_child root 0, Syntax.nth_child cur 0 with
  | Some c, Some e ->
    Printf.printf
      "  first child: K%d, %d bytes, at %d..%d\n"
      (Syntax.elem_kind e)
      (Green.child_text_len c)
      (fst (Syntax.elem_text_range e))
      (snd (Syntax.elem_text_range e))
  | _ -> ()
;;

let demo src =
  Printf.printf "\n=== source: %s ===\n" src;
  let cache, root = parse src in
  Printf.printf "  roundtrip ok? %b\n" (Green.to_source root = src);
  Format.printf "  cst:@\n    @[%a@]@." Green.pp root;
  show_shape root;
  let ir = lower_root root in
  Printf.printf "  lowered ast = %s\n" (Ir.to_string ir);
  (match eval_ir ir with
   | Some v -> Printf.printf "  eval (lowered) = %d\n" v
   | None -> Printf.printf "  eval (lowered) = <hole>\n");
  show_hover root (String.length src / 2);
  let folded = constant_fold cache root in
  Format.printf "  after fold:@\n    @[%a@]@." Green.pp folded;
  Printf.printf "  fold source = %S\n" (Green.to_source folded);
  Printf.printf
    "  cache: %d nodes, %d tokens (live)\n"
    Cache.((node_stats cache).entries)
    Cache.((token_stats cache).entries)
;;

(* ---- edits ---------------------------------------------------------------- *)

(* Four persistent edits against one tree. Each returns a fresh root, and the
   original is still there and still valid afterwards. *)

let demo_edits src =
  Printf.printf "\n=== edits: %s ===\n" src;
  let cache, root = parse src in
  let show label = function
    | None -> Printf.printf "  %-14s (not applicable)\n" label
    | Some (r : Syntax.t Syntax.result) ->
      Printf.printf "  %-14s %S\n" label (Syntax.to_source r.Syntax.root)
  in
  show "set literal 0" (set_literal cache root 0 99);
  show "swap operator" (swap_first_operator cache root K.star "*");
  show "parenthesise" (parenthesise cache root);
  show "drop first ws" (drop_first_ws cache root);
  Printf.printf "  original intact? %b\n" (Green.to_source root = src);
  (* Off-spine sharing: replacing literal 0 leaves the other literal's green
     node as the very same OCaml value. *)
  match set_literal cache root 0 99, Hashtbl.find_opt (literal_ptrs root) 1 with
  | Some r, Some p ->
    let old_cur = Syntax.of_root root in
    (match Syntax.Ptr.resolve_node old_cur p, Syntax.Ptr.resolve_node r.Syntax.root p with
     | Some before, Some after ->
       Printf.printf
         "  untouched literal shared? %b\n"
         (Syntax.green before == Syntax.green after);
       (* The same green value, but the edit above it widened the text, so the
          cursor sits at a different offset and [equal] says no. *)
       Printf.printf
         "  same root? %b   cursor equal (offset + tag)? %b\n"
         (Syntax.same_tree before after)
         (Syntax.equal before after);
       Format.printf "  edited self: @[%a@]@." Syntax.pp r.Syntax.self
     | _ -> ())
  | _ -> ()
;;

(* ---- ptrs ----------------------------------------------------------------- *)

(* The id map, and what a ptr can do without touching the tree. *)

let demo_ptrs src =
  Printf.printf "\n=== ptrs: %s ===\n" src;
  let _cache, root = parse src in
  let cur = Syntax.of_root root in
  let tbl = literal_ptrs root in
  let ids = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) tbl []) in
  List.iter
    (fun id ->
       let p = Hashtbl.find tbl id in
       Format.printf
         "  literal %d -> %a  (kind K%d, depth %d, hash %d)"
         id
         Syntax.Ptr.pp
         p
         (Syntax.Ptr.kind p)
         (Syntax.Ptr.depth p)
         (Syntax.Ptr.hash p land 0xffff);
       match Syntax.Ptr.resolve cur p with
       | Some (Syntax.Node n) -> Format.printf " = %S@." (Syntax.to_source n)
       | Some (Syntax.Token t) -> Format.printf " = %S@." (Syntax.Token.text t)
       | None -> Format.printf " (unresolved)@.")
    ids;
  match ids with
  | a :: b :: _ ->
    let pa = Hashtbl.find tbl a
    and pb = Hashtbl.find tbl b in
    Printf.printf
      "  ptr 0 vs 1: equal %b, compare %d, 0 ancestor-of 1 %b\n"
      (Syntax.Ptr.equal pa pb)
      (Syntax.Ptr.compare pa pb)
      (Syntax.Ptr.is_ancestor pa pb);
    (* A ptr taken through the token arm addresses the token, not its parent. *)
    (match Syntax.Ptr.resolve_node cur pa with
     | Some lit ->
       (match Syntax.nth_child lit 0 with
        | Some (Syntax.Token t as e) ->
          Printf.printf
            "  literal 0's token ptr: of_token = of_elem? %b, depth %d\n"
            (Syntax.Ptr.equal (Syntax.Ptr.of_token t) (Syntax.Ptr.of_elem e))
            (Syntax.Ptr.depth (Syntax.Ptr.of_token t))
        | Some (Syntax.Node _) | None -> ())
     | None -> ())
  | _ -> ()
;;

(* ---- cache modes ---------------------------------------------------------- *)

(* The same source through both modes. Hashconsed shares the two identical
   halves; Plain allocates each separately and pays no hash or bucket walk to
   do it. *)

let halves (root : Green.node) =
  match non_trivia root with
  | [ Green.Node e ] ->
    (match non_trivia e with
     | [ x; y; z ] ->
       (match x, y, z with
        | Green.Node a, Green.Token _, Green.Node b -> Some (a, b)
        | (Green.Node _ | Green.Token _), _, _ -> None)
     | [] | [ _ ] | [ _; _ ] | _ :: _ :: _ :: _ :: _ -> None)
  | [ Green.Token _ ] | [] | _ :: _ :: _ -> None
;;

let demo_cache_modes src =
  Printf.printf "\n=== cache modes: %s ===\n" src;
  let hashed, h_root = parse src in
  let plain_cache = Cache.create_plain () in
  let _, p_root = parse ~cache:plain_cache src in
  Printf.printf
    "  same source both ways? %b\n"
    (Green.to_source h_root = Green.to_source p_root);
  (match halves h_root, halves p_root with
   | Some (ha, hb), Some (pa, pb) ->
     Printf.printf "  hashconsed: identical halves shared? %b\n" (ha == hb);
     Printf.printf "  plain:      identical halves shared? %b\n" (pa == pb)
   | _ -> ());
  (* Token identity reads the same either way, since both modes draw their tags
     from the one global counter. *)
  let t1 = Green.mk_token hashed ~kind:K.int_lit ~text:"1" in
  let t2 = Green.mk_token hashed ~kind:K.int_lit ~text:"1" in
  Printf.printf
    "  two `1` tokens: equal %b, tags %d/%d\n"
    (Green.Token.equal t1 t2)
    (Green.Token.tag t1)
    (Green.Token.tag t2);
  Printf.printf "  live nodes before clear: %d\n" Cache.((node_stats hashed).entries);
  Cache.clear hashed;
  Printf.printf "  live nodes after clear:  %d\n" Cache.((node_stats hashed).entries);
  Printf.printf "  old root still readable? %b\n" (Green.to_source h_root = src)
;;

let () =
  let inputs =
    [ "1 + 2 * 3"
    ; "(1 + 2) * (3 + 4)"
    ; "((1+2)+(3+4)) + ((1+2)+(3+4))"
    ; "10 - 4 / 2"
    ; "1 + " (* missing rhs, so an ERROR slot *)
    ]
  in
  List.iter demo inputs;
  demo_edits "1 + 2 * 3";
  demo_ptrs "1 + 2 * 3";
  demo_cache_modes "((1+2)+(3+4)) + ((1+2)+(3+4))"
;;
