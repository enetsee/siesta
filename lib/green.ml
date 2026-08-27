(* Public green-tree surface.

   The types alias [Dedup]'s flat records, with [tag] inline, so equality and
   hash are a single field read. See [cache.mli] for what each cache mode means
   for sharing. *)

type token = Dedup.token
type node = Dedup.node

type child = Dedup.child =
  | Node of node
  | Token of token

let kind (n : node) = n.Dedup.nd_kind
let text_len (n : node) = n.Dedup.nd_text_len
let payload (n : node) = n.Dedup.nd_payload
let num_children (n : node) = Array.length n.Dedup.nd_children

let nth_child (n : node) i =
  let cs = n.Dedup.nd_children in
  if i < 0 || i >= Array.length cs then None else Some cs.(i)
;;

let children_array (n : node) = Array.copy n.Dedup.nd_children
let tag (n : node) = n.Dedup.nd_tag

let hash (n : node) =
  Hashtbl.seeded_hash
    (Hashtbl.seeded_hash n.Dedup.nd_kind n.Dedup.nd_text_len)
    n.Dedup.nd_payload
;;

let equal (a : node) (b : node) = a.Dedup.nd_tag = b.Dedup.nd_tag

module Token = struct
  let kind (t : token) = t.Dedup.tk_kind
  let text (t : token) = t.Dedup.tk_text
  let tag (t : token) = t.Dedup.tk_tag
  let equal (a : token) (b : token) = a.Dedup.tk_tag = b.Dedup.tk_tag
end

let child_text_len = function
  | Node n -> n.Dedup.nd_text_len
  | Token t -> String.length t.Dedup.tk_text
;;

let sum_text_len cs = Array.fold_left (fun acc c -> acc + child_text_len c) 0 cs
let mk_token cache ~kind ~text = Cache.hashcons_token cache ~kind ~text

let mk_node cache ~kind ?(payload = 0) ~children () =
  let text_len = sum_text_len children in
  Cache.hashcons_node cache ~kind ~text_len ~payload children
;;

let to_source (root : node) =
  let buf = Buffer.create (text_len root) in
  let stack = Stack.create () in
  Stack.push (Node root) stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | Token t -> Buffer.add_string buf t.Dedup.tk_text
    | Node n ->
      let cs = n.Dedup.nd_children in
      for i = Array.length cs - 1 downto 0 do
        Stack.push cs.(i) stack
      done
  done;
  Buffer.contents buf
;;

let pp ppf (root : node) =
  let stack = Stack.create () in
  Stack.push (`Visit (Node root)) stack;
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | `Close -> Format.fprintf ppf ")@]"
    | `Visit (Token t) -> Format.fprintf ppf "(K%d %S)" t.Dedup.tk_kind t.Dedup.tk_text
    | `Visit (Node n) ->
      let cs = n.Dedup.nd_children in
      if Array.length cs = 0
      then Format.fprintf ppf "(K%d)" n.Dedup.nd_kind
      else (
        Format.fprintf ppf "@[<v 2>(K%d" n.Dedup.nd_kind;
        Stack.push `Close stack;
        (* Children go on in reverse so they pop in source order, each
           preceded by a vertical break. *)
        for i = Array.length cs - 1 downto 0 do
          Stack.push (`Visit cs.(i)) stack;
          Stack.push `Cut stack
        done)
    | `Cut -> Format.fprintf ppf "@,"
  done
;;
