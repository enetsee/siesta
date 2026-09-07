(* Generated event scripts for the builder.

   The hand-written error cases in [test_siesta.ml] each walk one straight
   line: open a frame, do the wrong thing, expect [Failure]. They cover what
   somebody thought of. This file generates the scripts instead, legal and
   illegal mixed, and checks them against a reference model carrying only what
   the rules need: a stack of frames, the buffered children, and the
   checkpoints taken so far.

   Two properties:

   - [prop_legality]: the builder raises exactly when the model says the script
     is illegal, at the same event. The index has to agree too, since "raises
     somewhere" is satisfied by a builder that rejects far too much.
   - [prop_checkpoint_eq_direct]: for a legal script, the builder's tree equals
     the one you get by emitting the model's shape through plain
     [start_node] / [token] / [finish_node]. That is the general form of the
     left-associative case in [test_props.ml], for whatever shape the reuse
     takes. *)

open Siesta
module Gen = QCheck2.Gen
module Test = QCheck2.Test

let kinds = [| 10; 11; 12 |]
let tok_kinds = [| 1; 2 |]
let texts = [| "a"; "bb"; "" |]

type ev =
  | Start of int * int (* kind, payload *)
  | Tok of int * string
  | Finish (* finish_node *)
  | Mark (* checkpoint *)
  | Start_at of int * int * int (* which checkpoint, kind, payload *)

let pp_ev = function
  | Start (k, p) -> Printf.sprintf "Start(K%d,p%d)" k p
  | Tok (k, t) -> Printf.sprintf "Tok(K%d,%S)" k t
  | Finish -> "Finish"
  | Mark -> "Mark"
  | Start_at (i, k, p) -> Printf.sprintf "StartAt(#%d,K%d,p%d)" i k p
;;

let print_script evs = String.concat " " (List.map pp_ev evs)

(* Uniformly random events almost never spell a legal script: the census below
   measured 7 legal in 3000, which leaves "a legal script builds the tree it
   describes" nearly vacuous. So legal scripts are constructed, by only ever
   emitting an operation legal in the current state, and the illegal ones come
   from corrupting a legal script with a single edit. A near miss is the
   sharper test anyway; noise usually dies on its first event and never reaches
   the checkpoint rules at all. *)
let gen_ev_random =
  Gen.oneof_weighted
    [ 4, Gen.map2 (fun k p -> Start (k, p)) (Gen.oneof_array kinds) (Gen.int_range 0 2)
    ; ( 6
      , Gen.map2
          (fun k t -> Tok (k, t))
          (Gen.oneof_array tok_kinds)
          (Gen.oneof_array texts) )
    ; 5, Gen.return Finish
    ; 3, Gen.return Mark
    ; ( 3
      , Gen.map3
          (fun i k p -> Start_at (i, k, p))
          (Gen.int_range 0 3)
          (Gen.oneof_array kinds)
          (Gen.int_range 0 2) )
    ]
;;

let gen_random = Gen.list_size (Gen.int_range 1 24) gen_ev_random

(* -- the reference model --------------------------------------------------- *)

(* [pos] on a checkpoint is relative to its own frame, which comes to the same
   thing as the builder's absolute index: a checkpoint is only usable while its
   frame is on top, and the frames beneath it cannot move while that holds. *)

type shape =
  | S_tok of int * string
  | S_node of int * int * shape list

type frame =
  { f_kind : int
  ; f_payload : int
  ; f_gen : int
  ; mutable f_items : shape list (* reversed *)
  }

type mark =
  { m_gen : int
  ; m_pos : int
  ; mutable m_dead : bool
    (* Set by the wrap that strands this mark; see [step]'s [Start_at]. Kept on
       the mark rather than derived from a position on the frame because
       position cannot tell the two cases apart: a wrap at [p] leaves the frame
       [p + 1] items long, so a mark it stranded and a mark taken straight
       afterwards both sit at [p + 1]. *)
  }

type model =
  { mutable stack : frame list
  ; mutable root : shape option
  ; mutable next_gen : int
  ; mutable marks : mark list (* in the order taken *)
  }

exception Illegal

let fresh_gen m =
  let g = m.next_gen in
  m.next_gen <- g + 1;
  g
;;

let step m = function
  | Start (k, p) ->
    if Option.is_some m.root then raise Illegal;
    m.stack <- { f_kind = k; f_payload = p; f_gen = fresh_gen m; f_items = [] } :: m.stack
  | Tok (k, t) ->
    (match m.stack with
     | [] -> raise Illegal
     | top :: _ -> top.f_items <- S_tok (k, t) :: top.f_items)
  | Finish ->
    (match m.stack with
     | [] -> raise Illegal
     | top :: rest ->
       let node = S_node (top.f_kind, top.f_payload, List.rev top.f_items) in
       m.stack <- rest;
       (match rest with
        | parent :: _ -> parent.f_items <- node :: parent.f_items
        | [] -> m.root <- Some node))
  | Mark ->
    (match m.stack with
     | [] -> raise Illegal
     | top :: _ ->
       m.marks
       <- m.marks
          @ [ { m_gen = top.f_gen; m_pos = List.length top.f_items; m_dead = false } ])
  | Start_at (i, k, p) ->
    (match m.stack with
     | [] -> raise Illegal
     | top :: _ ->
       if m.marks = [] then raise Illegal;
       let cp = List.nth m.marks (i mod List.length m.marks) in
       if top.f_gen <> cp.m_gen then raise Illegal;
       let have = List.length top.f_items in
       (* A mark means "the items from here on are the ones that were here when
          I was taken". A [Start_at] at [p] swallows every item from [p] on, so
          it strands exactly this frame's marks that are further right *and had
          already been taken*; a mark taken afterwards describes the frame as
          that [Start_at] left it and is untouched.

          Written as an effect on the marks, which is the rule itself. Deriving
          it from a number on the frame is what both previous versions did, and
          both lost the "had already been taken" half. A model that borrows the
          implementation's shortcut cannot see the implementation miss. *)
       if cp.m_dead || cp.m_pos > have then raise Illegal;
       List.iter
         (fun mk -> if mk.m_gen = top.f_gen && mk.m_pos > cp.m_pos then mk.m_dead <- true)
         m.marks;
       (* The trailing [have - m_pos] items move into the new frame. *)
       let items = List.rev top.f_items in
       let keep = List.filteri (fun j _ -> j < cp.m_pos) items in
       let moved = List.filteri (fun j _ -> j >= cp.m_pos) items in
       top.f_items <- List.rev keep;
       m.stack
       <- { f_kind = k; f_payload = p; f_gen = fresh_gen m; f_items = List.rev moved }
          :: m.stack)
;;

(* [None] if the whole script plus a closing [finish] is legal, [Some i] for the
   first event that is not. Index [List.length evs] means the events were fine
   and [finish] was not. *)
let model_verdict evs =
  let m = { stack = []; root = None; next_gen = 0; marks = [] } in
  let rec go i = function
    | [] -> if m.stack <> [] || Option.is_none m.root then Some i else None
    | e :: rest ->
      (match step m e with
       | () -> go (i + 1) rest
       | exception Illegal -> Some i)
  in
  go 0 evs, m
;;

(* -- constructing a legal script -------------------------------------------- *)

(* Drive the model forward, choosing at each step only from the operations
   legal right now. Seeds stay a plain int list so the shrinker can still cut a
   failing script down. *)
let legal_options m =
  match m.stack with
  | [] -> if Option.is_some m.root then [] else [ `Start ]
  | top :: _ ->
    let reuses =
      List.mapi (fun i cp -> i, cp) m.marks
      |> List.filter (fun (_, cp) ->
        cp.m_gen = top.f_gen && (not cp.m_dead) && cp.m_pos <= List.length top.f_items)
      |> List.map (fun (i, _) -> `Start_at i)
    in
    [ `Start; `Tok; `Finish; `Mark ] @ reuses
;;

let script_of_seeds seeds =
  let m = { stack = []; root = None; next_gen = 0; marks = [] } in
  let out = ref [] in
  let emit1 e =
    step m e;
    out := e :: !out
  in
  emit1 (Start (kinds.(0), 0));
  List.iter
    (fun seed ->
       let opts = legal_options m in
       if opts <> []
       then (
         let k = kinds.(seed / 7 mod Array.length kinds) in
         let p = seed / 11 mod 3 in
         match List.nth opts (seed mod List.length opts) with
         | `Start -> emit1 (Start (k, p))
         | `Tok ->
           emit1
             (Tok
                ( tok_kinds.(seed / 13 mod Array.length tok_kinds)
                , texts.(seed / 17 mod Array.length texts) ))
         | `Finish -> emit1 Finish
         | `Mark -> emit1 Mark
         | `Start_at i -> emit1 (Start_at (i, k, p))))
    seeds;
  (* Close whatever is still open, so the script ends on a complete tree. *)
  List.iter (fun () -> emit1 Finish) (List.init (List.length m.stack) (fun _ -> ()));
  List.rev !out
;;

let gen_legal =
  Gen.map script_of_seeds (Gen.list_size (Gen.int_range 0 30) (Gen.int_range 0 9999))
;;

(* One edit to a legal script: drop an event, repeat one, swap one for a
   different operation, or re-point a checkpoint reuse at a different mark.

   That last mode is the one that matters. [legal_options] only ever offers a
   checkpoint that is still valid, so a constructed-legal script never contains
   a stale reuse and the corpus never reaches the stale-position rule. The
   census read 0/3000 on that rejection until this mode existed. *)
let start_at_indices evs =
  List.filteri
    (fun _ (_, e) ->
       match e with
       | Start_at _ -> true
       | Start _ | Tok _ | Finish | Mark -> false)
    (List.mapi (fun i e -> i, e) evs)
  |> List.map fst
;;

let gen_corrupted =
  Gen.map3
    (fun evs where how ->
       let n = List.length evs in
       if n = 0
       then evs
       else (
         let i = where mod n in
         match how mod 4 with
         | 0 -> List.filteri (fun j _ -> j <> i) evs
         | 1 -> List.concat (List.mapi (fun j e -> if j = i then [ e; e ] else [ e ]) evs)
         | 2 ->
           List.mapi
             (fun j e ->
                if j <> i
                then e
                else (
                  match e with
                  | Finish -> Mark
                  | Mark -> Finish
                  | Start (k, p) -> Start_at (where / 3, k, p)
                  | Start_at (_, k, p) -> Start (k, p)
                  | Tok _ -> Finish))
             evs
         | _ ->
           (match start_at_indices evs with
            | [] -> evs
            | idxs ->
              let target = List.nth idxs (where mod List.length idxs) in
              List.mapi
                (fun j e ->
                   if j <> target
                   then e
                   else (
                     match e with
                     | Start_at (m, k, p) -> Start_at (m + 1 + (how / 4 mod 3), k, p)
                     | (Start _ | Tok _ | Finish | Mark) as other -> other))
                evs)))
    gen_legal
    (Gen.int_range 0 1000)
    (Gen.int_range 0 1000)
;;

(* Two marks in one frame, the earlier reused first.

   Re-pointing alone does not reach this. Mark indices are global, so a
   re-pointed reuse almost always names a mark from another frame and the gen
   guard answers before the position rule runs, hence building the shape
   directly. Reusing mark 0 wraps everything from position 0 and leaves the
   frame holding a single node, so mark 1, taken at position [pre] >= 2, now
   points past the end while still carrying the frame's gen. That is the defect
   the stale-position guard was added for. *)
let gen_stale_pattern =
  Gen.map2
    (fun a b ->
       let toks n =
         List.init n (fun i ->
           Tok (tok_kinds.(i mod Array.length tok_kinds), texts.(i mod Array.length texts)))
       in
       let pre = 2 + (a mod 3)
       and post = b mod 3 in
       List.concat
         [ [ Start (kinds.(0), 0); Mark ]
         ; toks pre
         ; [ Mark ]
         ; toks post
         ; [ Start_at (0, kinds.(1), 0); Finish; Start_at (1, kinds.(2), 0) ]
         ])
    (Gen.int_range 0 100)
    (Gen.int_range 0 100)
;;

let gen_script =
  Gen.oneof_weighted
    [ 3, gen_legal; 4, gen_corrupted; 1, gen_random; 1, gen_stale_pattern ]
;;

(* -- the builder under test ------------------------------------------------ *)

let builder_verdict ?cache evs =
  let b = Builder.create ?cache () in
  let marks = ref [] in
  let rec go i = function
    | [] ->
      (match Builder.finish b with
       | tree -> None, Some tree
       | exception Failure _ -> Some i, None)
    | e :: rest ->
      let act () =
        match e with
        | Start (k, p) -> Builder.start_node b ~payload:p k
        | Tok (k, t) -> Builder.token b k t
        | Finish -> Builder.finish_node b
        | Mark -> marks := !marks @ [ Builder.checkpoint b ]
        | Start_at (i, k, p) ->
          if !marks = [] then failwith "no checkpoint taken yet";
          Builder.start_node_at
            b
            ~payload:p
            (List.nth !marks (i mod List.length !marks))
            k
      in
      (match act () with
       | () -> go (i + 1) rest
       | exception Failure _ -> Some i, None)
  in
  go 0 evs
;;

(* -- legal exactly when the model says so ----------------------------------- *)

let prop_legality =
  Test.make
    ~name:"builder accepts a script exactly when the model does, at the same event"
    ~count:2000
    ~print:print_script
    gen_script
    (fun evs ->
       let expected, _ = model_verdict evs in
       let got, _ = builder_verdict evs in
       expected = got)
;;

(* -- the harder half: checkpoint reuse against direct nesting --------------- *)

let rec emit b = function
  | S_tok (k, t) -> Builder.token b k t
  | S_node (k, p, cs) ->
    Builder.start_node b ~payload:p k;
    List.iter (emit b) cs;
    Builder.finish_node b
;;

let prop_checkpoint_eq_direct =
  Test.make
    ~name:"checkpoint reuse builds what the equivalent direct nesting builds"
    ~count:2000
    ~print:print_script
    gen_legal
    (fun evs ->
       match model_verdict evs with
       | Some _, _ -> true (* nothing legal survived, so nothing to compare *)
       | None, m ->
         let cache = Cache.create () in
         (match builder_verdict ~cache evs with
          | None, Some via_events ->
            let via_direct =
              let b = Builder.create ~cache () in
              emit b (Option.get m.root);
              Builder.finish b
            in
            via_events == via_direct
          | _ -> false))
;;

(* -- what the script corpus actually contained ------------------------------ *)

(* Both properties are vacuous on a corpus that is all legal or all illegal,
   and [prop_checkpoint_eq_direct] says nothing unless reuses survive the
   truncation. So it is counted rather than assumed. *)
let test_script_census () =
  let n = 3000 in
  let rand = Random.State.make [| 0xB0176 |] in
  let legal = ref 0
  and illegal = ref 0
  and with_reuse = ref 0
  and legal_with_reuse = ref 0
  and stale_rejected = ref 0
  and deepest = ref 0 in
  for _ = 1 to n do
    let evs = Gen.generate1 ~rand gen_script in
    let verdict, _ = model_verdict evs in
    let has_reuse =
      List.exists
        (function
          | Start_at _ -> true
          | Start _ | Tok _ | Finish | Mark -> false)
        evs
    in
    if has_reuse then incr with_reuse;
    (match verdict with
     | None ->
       incr legal;
       if has_reuse then incr legal_with_reuse
     | Some _ -> incr illegal);
    (* How often the stale-position rule is the one that fires, as opposed to
       the gen guard or an empty stack. *)
    let m = { stack = []; root = None; next_gen = 0; marks = [] } in
    let rec walk = function
      | [] -> ()
      | e :: rest ->
        let stale_now =
          match m.stack, e with
          | top :: _, Start_at (i, _, _) when m.marks <> [] ->
            let cp = List.nth m.marks (i mod List.length m.marks) in
            top.f_gen = cp.m_gen && (cp.m_dead || cp.m_pos > List.length top.f_items)
          | [], _ | _ :: _, (Start _ | Tok _ | Finish | Mark | Start_at _) -> false
        in
        (match step m e with
         | () ->
           if List.length m.stack > !deepest then deepest := List.length m.stack;
           walk rest
         | exception Illegal -> if stale_now then incr stale_rejected)
    in
    walk evs
  done;
  let at_least what floor got =
    Alcotest.(check bool)
      (Printf.sprintf "%s: %d/%d (floor %d)" what got n floor)
      true
      (got >= floor)
  in
  at_least "legal scripts" (n / 5) !legal;
  at_least "illegal scripts" (n / 5) !illegal;
  at_least "scripts using a checkpoint reuse" (n / 5) !with_reuse;
  (* The one that matters: a reuse inside a script that actually builds a tree,
     which is what [prop_checkpoint_eq_direct] needs to say anything. *)
  at_least "legal scripts using a checkpoint reuse" (n / 10) !legal_with_reuse;
  at_least "rejections that are the stale-position guard" 1 !stale_rejected;
  Alcotest.(check bool)
    (Printf.sprintf "deepest frame stack seen was %d" !deepest)
    true
    (!deepest >= 3)
;;

let () =
  Alcotest.run
    "builder-script"
    [ "law 3", [ Helpers.qcheck prop_legality; Helpers.qcheck prop_checkpoint_eq_direct ]
    ; "corpus", [ Alcotest.test_case "census" `Quick test_script_census ]
    ]
;;
