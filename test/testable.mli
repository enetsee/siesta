(** [Alcotest.testable] values for siesta's tree types.

    A testable pairs a printer with an equality so alcotest can show a readable
    diff when [check] fails. For physical-sharing assertions reach for
    {!Helpers.same} / {!Helpers.distinct}, which can express [==]. *)

open Siesta

(** Green nodes, printed with {!Green.pp}, compared by {!Green.equal}. That is
    hash-cons identity, so it means structural equality only within one cache
    and between clears. *)
val green : Green.node Alcotest.testable

val green_token : Green.token Alcotest.testable

(** Interned {!Dedup.token} records, compared on every field, tag included. *)
val dedup_token : Dedup.token Alcotest.testable

(** Interned {!Dedup.node} records, compared on the scalar fields and the child
    count rather than deep structure, which sharing has already collapsed. *)
val dedup_node : Dedup.node Alcotest.testable
