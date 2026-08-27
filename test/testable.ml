(* [Alcotest.testable] values for siesta's tree types. Documented in
   testable.mli. *)

open Siesta

(* Green nodes, printed with {!Green.pp}, compared by {!Green.equal}. *)
let green : Green.node Alcotest.testable = Alcotest.testable Green.pp Green.equal

let pp_green_token ppf t =
  Format.fprintf ppf "(K%d %S)" (Green.Token.kind t) (Green.Token.text t)
;;

let green_token : Green.token Alcotest.testable =
  Alcotest.testable pp_green_token Green.Token.equal
;;

(* Interned {!Dedup.token} records, compared on every field. Tag included:
   two equal interns share one. *)
let dedup_token : Dedup.token Alcotest.testable =
  let pp ppf (t : Dedup.token) =
    Dedup.(Format.fprintf ppf "{tag=%d; kind=%d; text=%S}" t.tk_tag t.tk_kind t.tk_text)
  in
  let equal (a : Dedup.token) (b : Dedup.token) =
    Dedup.(
      a.tk_tag = b.tk_tag && a.tk_kind = b.tk_kind && String.equal a.tk_text b.tk_text)
  in
  Alcotest.testable pp equal
;;

(* Interned {!Dedup.node} records, compared on the scalar fields and the child
   count. Sharing has already collapsed the deep structure. *)
let dedup_node : Dedup.node Alcotest.testable =
  let pp ppf (n : Dedup.node) =
    Dedup.(
      Format.fprintf
        ppf
        "{tag=%d; kind=%d; text_len=%d; payload=%d; nchildren=%d}"
        n.nd_tag
        n.nd_kind
        n.nd_text_len
        n.nd_payload
        (Array.length n.nd_children))
  in
  let equal (a : Dedup.node) (b : Dedup.node) =
    Dedup.(
      a.nd_tag = b.nd_tag
      && a.nd_kind = b.nd_kind
      && a.nd_text_len = b.nd_text_len
      && a.nd_payload = b.nd_payload
      && Array.length a.nd_children = Array.length b.nd_children)
  in
  Alcotest.testable pp equal
;;
