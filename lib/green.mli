(** Green tree: the lossless, hash-consed representation.

    Internal interface. Everything re-exported by {!Siesta.Green} is documented
    in [siesta.mli]; only what stays private to the library is described here. *)

type token
type node

type child =
  | Node of node
  | Token of token

val kind : node -> int
val text_len : node -> int
val payload : node -> int
val tag : node -> int
val equal : node -> node -> bool
val num_children : node -> int
val nth_child : node -> int -> child option

(** [nth_child] without the option, for callers that have already bounded the
    index. Raises [Invalid_argument] out of range. *)
val nth_child_exn : node -> int -> child

val children_array : node -> child array

module Token : sig
  val kind : token -> int
  val text : token -> string
  val tag : token -> int
  val equal : token -> token -> bool
end

val mk_token : Cache.t -> kind:int -> text:string -> token
val mk_token_slice : Cache.t -> kind:int -> src:string -> pos:int -> len:int -> token
val mk_node : Cache.t -> kind:int -> ?payload:int -> children:child array -> unit -> node

(** [mk_node] over [children.(pos)] to [children.(pos + len - 1)], for the
    builder, which holds a node's children in a range of its buffer. Not
    re-exported by {!Siesta.Green}. *)
val mk_node_sub
  :  Cache.t
  -> kind:int
  -> payload:int
  -> child array
  -> pos:int
  -> len:int
  -> node

val child_text_len : child -> int
val sum_text_len : child array -> int
val to_source : node -> string
val pp : Format.formatter -> node -> unit
