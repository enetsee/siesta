(** Red (cursor) layer.

    Internal interface. The public surface is documented in full on
    {!Siesta.Syntax}; see [siesta.mli]. *)

type t
type token_cursor

type elem =
  | Node of t
  | Token of token_cursor

type 'a result =
  { root : 'a
  ; self : 'a
  }

val of_root : Green.node -> t
val kind : t -> int
val green : t -> Green.node
val parent : t -> t option
val index_in_parent : t -> int
val text_range : t -> int * int
val equal : t -> t -> bool
val same_tree : t -> t -> bool
val children_array : t -> elem array
val nth_child : t -> int -> elem option
val to_source : t -> string
val pp : Format.formatter -> t -> unit
val elem_kind : elem -> int
val elem_text_range : elem -> int * int
val ancestors : t -> t Seq.t

(* [Token.parent] refers to the outer cursor [t], so these are typed in terms of
   [token_cursor]. OCaml has no tidy way to write that mutual reference inside a
   nested signature. *)
module Token : sig
  val kind : token_cursor -> int
  val text : token_cursor -> string
  val green : token_cursor -> Green.token
  val parent : token_cursor -> t
  val index_in_parent : token_cursor -> int
  val text_range : token_cursor -> int * int
  val equal : token_cursor -> token_cursor -> bool
end

val token_at_offset : t -> int -> token_cursor option
val node_at_offset : t -> int -> t option

type visit =
  | Descend
  | Skip

val preorder : t -> f:(t -> visit) -> unit
val descendants : t -> t Seq.t

module Ptr : sig
  (* The outer node cursor, bound before [t] below shadows it. *)
  type cursor := t
  type t

  val of_node : cursor -> t
  val of_token : token_cursor -> t
  val of_elem : elem -> t
  val resolve : cursor -> t -> elem option
  val resolve_node : cursor -> t -> cursor option
  val kind : t -> int
  val depth : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val is_ancestor : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

val replace : Cache.t -> t -> Green.node -> t result
val splice_children : Cache.t -> t -> at:int -> remove:int -> Green.child list -> t result
val replace_child : Cache.t -> elem -> Green.child -> t result
val splice_at : Cache.t -> elem -> remove:int -> Green.child list -> t result
