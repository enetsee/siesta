open Siesta

val mk_tok : Cache.t -> int -> string -> Green.token
val mk_node : Cache.t -> ?payload:int -> int -> Green.child array -> Green.node

(** [same msg a b] passes iff [a] and [b] are physically equal, the sharing
    guarantee hash-consing exists to provide. *)
val same : string -> 'a -> 'a -> unit

(** [distinct msg a b] passes iff [a] and [b] are separate cache entries. *)
val distinct : string -> 'a -> 'a -> unit

(** Lift a [QCheck2.Test.t] into an alcotest case. *)
val qcheck : QCheck2.Test.t -> unit Alcotest.test_case
