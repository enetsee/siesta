(** Flat green-record types and the weak dedup tables behind them.

    Internal interface. The whole surface is re-exported by {!Siesta.Dedup} and
    documented there; see [siesta.mli]. *)

type token =
  { tk_tag : int
  ; tk_kind : int
  ; tk_text : string
  }

type node =
  { nd_tag : int
  ; nd_kind : int
  ; nd_text_len : int
  ; nd_children : child array
  ; nd_payload : int
  }

and child =
  | Node of node
  | Token of token

type token_t

val token_create : ?capacity:int -> unit -> token_t
val token_intern : token_t -> kind:int -> text:string -> token
val token_clear : token_t -> unit

type node_t

val node_create : ?capacity:int -> unit -> node_t
val node_intern : node_t -> kind:int -> text_len:int -> payload:int -> child array -> node
val node_clear : node_t -> unit
val fresh_tag : unit -> int
val token_stats : token_t -> int * int * int * int * int * int
val node_stats : node_t -> int * int * int * int * int * int
