open Siesta

let mk_tok cache k text = Green.mk_token cache ~kind:k ~text

let mk_node cache ?(payload = 0) k children =
  Green.mk_node cache ~kind:k ~payload ~children ()
;;

(** [same msg a b] passes iff [a] and [b] are physically equal, the sharing
    guarantee hash-consing exists to provide. *)
let same msg a b = Alcotest.(check bool) msg true (a == b)

(** [distinct msg a b] passes iff [a] and [b] are separate cache entries. *)
let distinct msg a b = Alcotest.(check bool) msg true (not (a == b))

(** Lift a [QCheck2.Test.t] into an alcotest test case. *)
let qcheck (test : QCheck2.Test.t) : unit Alcotest.test_case =
  QCheck_alcotest.to_alcotest ~speed_level:`Quick test
;;
