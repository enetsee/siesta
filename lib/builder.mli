(** Event-driven green-tree builder.

    Internal interface. The public surface is documented in full on
    {!Siesta.Builder}; see [siesta.mli]. *)

type t
type checkpoint

val create : ?cache:Cache.t -> ?initial_children_capacity:int -> unit -> t
val start_node : t -> ?payload:int -> int -> unit
val token : t -> int -> string -> unit
val finish_node : t -> unit
val finish : t -> Green.node
val checkpoint : t -> checkpoint
val start_node_at : t -> ?payload:int -> checkpoint -> int -> unit
