(** Hash-cons cache.

    Internal interface. Everything re-exported by {!Siesta.Cache} is documented
    in [siesta.mli]; only what stays private to the library is described here. *)

type t

val create : ?capacity:int -> unit -> t
val create_synchronized : ?capacity:int -> unit -> t
val create_plain : unit -> t

(** Not re-exported by {!Siesta.Cache}. Gives a token its identity: in
    Hashconsed mode structurally-equal tokens share one record, in Plain mode
    every call allocates. Public callers go through {!Green.mk_token}. *)
val hashcons_token : t -> kind:int -> text:string -> Dedup.token

(** The same, for text that is still a slice of a larger string. Public callers
    go through {!Green.mk_token_slice}. *)
val hashcons_token_slice
  :  t
  -> kind:int
  -> src:string
  -> pos:int
  -> len:int
  -> Dedup.token

(** Not re-exported by {!Siesta.Cache}. Splits on Hashconsed and Plain exactly
    as {!hashcons_token} does. The children are [children.(pos)] to
    [children.(pos + len - 1)], copied out only when a new node is made. Public
    callers go through {!Green.mk_node}. *)
val hashcons_node
  :  t
  -> kind:int
  -> text_len:int
  -> payload:int
  -> Dedup.child array
  -> pos:int
  -> len:int
  -> Dedup.node

val clear : t -> unit

type stats =
  { table_length : int
  ; entries : int
  ; sum_bucket_lengths : int
  ; smallest_bucket : int
  ; median_bucket : int
  ; biggest_bucket : int
  }

val token_stats : t -> stats
val node_stats : t -> stats
