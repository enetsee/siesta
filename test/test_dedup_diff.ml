(* Differential test: replay random dedup scripts against [Siesta.Dedup] and
   against the plain [Strong] reference below, then compare the equivalence
   classes the tags induce. Two inputs that dedup together in one have to dedup
   together in the other, so long as no GC fires in between. *)

(* -- the strong-ref reference ---------------------------------------------- *)

module Strong : sig
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
  type node_t

  val token_create : unit -> token_t
  val node_create : unit -> node_t
  val token_intern : token_t -> kind:int -> text:string -> token

  val node_intern
    :  node_t
    -> kind:int
    -> text_len:int
    -> payload:int
    -> child array
    -> node
end = struct
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

  let next_tag = ref 0

  let fresh_tag () =
    incr next_tag;
    !next_tag
  ;;

  type token_t = (int * string, token) Hashtbl.t
  type node_t = (int * int * int * int array, node) Hashtbl.t

  let token_create () = Hashtbl.create 256
  let node_create () = Hashtbl.create 256

  let token_intern t ~kind ~text =
    match Hashtbl.find_opt t (kind, text) with
    | Some existing -> existing
    | None ->
      let v = { tk_tag = fresh_tag (); tk_kind = kind; tk_text = text } in
      Hashtbl.add t (kind, text) v;
      v
  ;;

  (* Children keyed by tag plus a node/token bit, matching [Dedup.child_key]. *)
  let child_key = function
    | Node n -> (n.nd_tag lsl 1) lor 0
    | Token t -> (t.tk_tag lsl 1) lor 1
  ;;

  let node_intern t ~kind ~text_len ~payload children =
    let key = kind, text_len, payload, Array.map child_key children in
    match Hashtbl.find_opt t key with
    | Some existing -> existing
    | None ->
      let v =
        { nd_tag = fresh_tag ()
        ; nd_kind = kind
        ; nd_text_len = text_len
        ; nd_children = children
        ; nd_payload = payload
        }
      in
      Hashtbl.add t key v;
      v
  ;;
end

(* -- replay both backends, compare equivalence classes --------------------- *)

(* A script of intern operations. Every event runs against both backends, and
   for each pair of events the two have to agree on whether the tags match. *)
type event =
  | Tok of int * string (* kind, text *)
  | Nod of int * int * int * int array (* kind, text_len, payload, child-indices *)

(* Children point back at earlier events, so the first 30 are all tokens to
   give the node events something to reference. *)
let gen_script ~n ~rng : event array =
  let script = Array.make n (Tok (0, "")) in
  for i = 0 to n - 1 do
    if i < 30 || Random.State.int rng 3 = 0
    then
      script.(i)
      <- Tok (Random.State.int rng 5, Printf.sprintf "t%d" (Random.State.int rng 30))
    else (
      let arity = 1 + Random.State.int rng 4 in
      let children = Array.init arity (fun _ -> Random.State.int rng i) in
      script.(i)
      <- Nod
           ( Random.State.int rng 5
           , Random.State.int rng 100
           , Random.State.int rng 3
           , children ))
  done;
  script
;;

(* Run script through Dedup, return per-event tag. *)
let replay_dedup (script : event array) : int array =
  let tt = Siesta.Dedup.token_create () in
  let nt = Siesta.Dedup.node_create () in
  let n = Array.length script in
  let tags = Array.make n 0 in
  (* Per-event handles, so a node event can find its children by index. *)
  let handles = Array.make n (Obj.magic 0 : Siesta.Dedup.child) in
  for i = 0 to n - 1 do
    match script.(i) with
    | Tok (kind, text) ->
      let v = Siesta.Dedup.token_intern tt ~kind ~text in
      tags.(i) <- v.Siesta.Dedup.tk_tag;
      handles.(i) <- Siesta.Dedup.Token v
    | Nod (kind, text_len, payload, child_ids) ->
      let children = Array.map (fun idx -> handles.(idx)) child_ids in
      let v = Siesta.Dedup.node_intern nt ~kind ~text_len ~payload children in
      tags.(i) <- v.Siesta.Dedup.nd_tag;
      handles.(i) <- Siesta.Dedup.Node v
  done;
  tags
;;

(* Run script through Strong reference, return per-event tag. *)
let replay_strong (script : event array) : int array =
  let tt = Strong.token_create () in
  let nt = Strong.node_create () in
  let n = Array.length script in
  let tags = Array.make n 0 in
  let handles = Array.make n (Obj.magic 0 : Strong.child) in
  for i = 0 to n - 1 do
    match script.(i) with
    | Tok (kind, text) ->
      let v = Strong.token_intern tt ~kind ~text in
      tags.(i) <- v.Strong.tk_tag;
      handles.(i) <- Strong.Token v
    | Nod (kind, text_len, payload, child_ids) ->
      let children = Array.map (fun idx -> handles.(idx)) child_ids in
      let v = Strong.node_intern nt ~kind ~text_len ~payload children in
      tags.(i) <- v.Strong.nd_tag;
      handles.(i) <- Strong.Node v
  done;
  tags
;;

(* The two backends draw from separate counters, so the comparison is on the
   partition the tags induce over event indices. *)
let equiv_classes (tags : int array) : int array array =
  let by_tag = Hashtbl.create 16 in
  Array.iteri
    (fun idx tag ->
       let prev =
         try Hashtbl.find by_tag tag with
         | Not_found -> []
       in
       Hashtbl.replace by_tag tag (idx :: prev))
    tags;
  let classes = Hashtbl.fold (fun _ indices acc -> indices :: acc) by_tag [] in
  let classes =
    List.map
      (fun indices ->
         let arr = Array.of_list indices in
         Array.sort compare arr;
         arr)
      classes
  in
  let classes_arr = Array.of_list classes in
  Array.sort
    (fun a b ->
       if Array.length a = 0 || Array.length b = 0
       then compare (Array.length a) (Array.length b)
       else compare a.(0) b.(0))
    classes_arr;
  classes_arr
;;

let classes_equal (a : int array array) (b : int array array) : bool =
  Array.length a = Array.length b
  &&
  let n = Array.length a in
  let rec all_eq i =
    if i >= n
    then true
    else (
      let ai = a.(i) in
      let bi = b.(i) in
      if Array.length ai <> Array.length bi
      then false
      else (
        let m = Array.length ai in
        let rec inner j =
          if j >= m then true else if ai.(j) <> bi.(j) then false else inner (j + 1)
        in
        inner 0 && all_eq (i + 1)))
  in
  all_eq 0
;;

(* -- driver ---------------------------------------------------------------- *)

let test_one_run ~n ~seed =
  let rng = Random.State.make [| seed |] in
  let script = gen_script ~n ~rng in
  (* A big minor heap keeps the GC quiet through the run. The two backends only
     agree while nothing has been collected. *)
  let saved = Gc.get () in
  let no_collect = { saved with Gc.minor_heap_size = 16 * 1024 * 1024 } in
  Gc.set no_collect;
  let dedup_tags = replay_dedup script in
  let strong_tags = replay_strong script in
  Gc.set saved;
  let dedup_classes = equiv_classes dedup_tags in
  let strong_classes = equiv_classes strong_tags in
  if not (classes_equal dedup_classes strong_classes)
  then Alcotest.failf "equivalence classes disagree at n=%d, seed=%d" n seed
;;

(* 1000 short scripts: broad coverage of resize / collision paths. *)
let test_short_scripts () =
  for seed = 0 to 999 do
    test_one_run ~n:200 ~seed
  done
;;

(* A handful of very long scripts: stresses the tables at scale. *)
let test_long_scripts () =
  for seed = 100_000 to 100_004 do
    test_one_run ~n:20_000 ~seed
  done
;;

let () =
  Alcotest.run
    "dedup_diff"
    [ ( "differential vs Strong reference"
      , [ Alcotest.test_case "1000 scripts × 200 events" `Quick test_short_scripts
        ; Alcotest.test_case "5 scripts × 20K events" `Quick test_long_scripts
        ] )
    ]
;;
