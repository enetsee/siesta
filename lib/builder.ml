(* One process-wide counter, so a checkpoint can name the builder that issued
   it. A frame's [gen] is only unique within a builder and every builder starts
   counting from 0, so without this a checkpoint handed to a second builder
   matches a frame that has nothing to do with it and wraps whatever happens to
   sit at its offset. Drawn once per builder rather than once per frame:
   [start_node] is on the hot path and [gen] already separates frames there. *)
let builder_counter = Atomic.make 0
let fresh_builder_id () = Atomic.fetch_and_add builder_counter 1

type checkpoint =
  { builder_id : int
  ; frame_gen : int
  ; pos : int
  ; stamp : int
    (* The builder's clock when this checkpoint was taken. Every
       [start_node_at] is stamped off the same clock, which is what lets a later
       one tell a call that ran before this checkpoint from a call that ran
       after. Position alone cannot: a [start_node_at] at [p] leaves the buffer
       [p + 1] long, so a checkpoint it stranded and a checkpoint taken
       immediately afterwards both sit at [p + 1]. *)
  }

(* One [start_node_at] that has already run on a frame. [sa_pos] is the child
   position it started the new node at, so it took every child from there
   onwards into that node, and [sa_stamp] is the builder clock when it ran.
   Together they say which checkpoints it stranded: those sitting to the right
   of [sa_pos] that were taken before [sa_stamp].

   Prefixed because [checkpoint] above carries a position and a stamp too and
   they mean different things: these are where the call cut and when it ran, a
   checkpoint's are where it points and when it was taken. Same convention as
   [Dedup]'s [tk_] and [nd_]. *)
type start_at =
  { sa_pos : int
  ; sa_stamp : int
  }

(* Mutable so a record can be reused: the frames live in [t.frames] and a push
   overwrites the record at that depth, so an open node allocates nothing once
   the stack has been that deep before. Nothing outside the builder holds a
   frame, and a checkpoint names its frame by [gen], which every push draws
   fresh, so a reused record cannot be mistaken for the frame it replaced. *)
type frame =
  { mutable kind : int
  ; mutable payload : int
  ; mutable gen : int
  ; mutable children_start : int
  ; mutable start_ats : start_at list
    (* Newest first, empty until [start_node_at] has run on this frame at all.
       Pruned on insert so positions decrease strictly towards the tail; see
       [record_start_at] for why that is sound and [stranded] for what reads
       it. *)
  }

type t =
  { mutable frames : frame array
  ; mutable depth : int (* open frames are [frames.(0)] to [frames.(depth - 1)] *)
  ; mutable root : Green.node option
  ; mutable clock : int
  ; id : int
  ; cache : Cache.t
  ; children : Green.child Dynarray.t
  }

let create ?cache ?(initial_children_capacity = 64) () =
  let cache =
    match cache with
    | Some c -> c
    | None -> Cache.create ()
  in
  let children = Dynarray.create () in
  Dynarray.ensure_capacity children initial_children_capacity;
  { frames = [||]
  ; depth = 0
  ; root = None
  ; clock = 0
  ; id = fresh_builder_id ()
  ; cache
  ; children
  }
;;

let cache t = t.cache

(* Frame gens and checkpoint stamps both come off this one counter. A gen is
   only ever compared for equality and a stamp only for order, so sharing it
   costs nothing and keeps "later" meaning the same thing for both. *)
let tick t =
  let c = t.clock in
  t.clock <- c + 1;
  c
;;

let new_frame () = { kind = 0; payload = 0; gen = 0; children_start = 0; start_ats = [] }

(* Doubles, filling the new slots with fresh records so every slot below the
   length always holds one to reuse. *)
let grow_frames t =
  let old = t.frames in
  let n = Array.length old in
  let n' = if n = 0 then 16 else 2 * n in
  t.frames <- Array.init n' (fun i -> if i < n then old.(i) else new_frame ())
;;

let push_frame t ~kind ~payload ~children_start =
  if t.depth = Array.length t.frames then grow_frames t;
  let f = t.frames.(t.depth) in
  f.kind <- kind;
  f.payload <- payload;
  f.gen <- tick t;
  f.children_start <- children_start;
  f.start_ats <- [];
  t.depth <- t.depth + 1
;;

let start_node t ?(payload = 0) kind =
  if Option.is_some t.root then failwith "Builder.start_node: tree already finished";
  push_frame t ~kind ~payload ~children_start:(Dynarray.length t.children)
;;

let token t kind text =
  if t.depth > 0
  then (
    let tok = Green.mk_token t.cache ~kind ~text in
    Dynarray.add_last t.children (Green.Token tok))
  else if Option.is_some t.root
  then failwith "Builder.token: tree already finished"
  else failwith "Builder.token: no node started"
;;

let token_slice t kind src pos len =
  if t.depth > 0
  then (
    let tok = Green.mk_token_slice t.cache ~kind ~src ~pos ~len in
    Dynarray.add_last t.children (Green.Token tok))
  else if Option.is_some t.root
  then failwith "Builder.token_slice: tree already finished"
  else failwith "Builder.token_slice: no node started"
;;

(* Lift a closing frame's children off the shared buffer as an array of their
   own, ready for a green node. *)
let pop_children_array t (children_start : int) : Green.child array =
  let len = Dynarray.length t.children - children_start in
  if len <= 0
  then [||]
  else (
    let arr = Array.make len (Dynarray.get t.children children_start) in
    for i = 1 to len - 1 do
      arr.(i) <- Dynarray.get t.children (children_start + i)
    done;
    Dynarray.truncate t.children children_start;
    arr)
;;

let finish_node t =
  if t.depth > 0
  then (
    let top = t.frames.(t.depth - 1) in
    let children = pop_children_array t top.children_start in
    let node = Green.mk_node t.cache ~kind:top.kind ~payload:top.payload ~children () in
    t.depth <- t.depth - 1;
    if t.depth > 0
    then Dynarray.add_last t.children (Green.Node node)
    else t.root <- Some node)
  else if Option.is_some t.root
  then failwith "Builder.finish_node: tree already finished"
  else failwith "Builder.finish_node: no node started"
;;

let finish t =
  if t.depth > 0
  then failwith "Builder.finish: unbalanced start/finish"
  else (
    match t.root with
    | Some r -> r
    | None -> failwith "Builder.finish: nothing built")
;;

(* -- checkpoints ----------------------------------------------------------- *)

let checkpoint t =
  if t.depth > 0
  then
    (* [pos] is the same [children_start] a frame opened here would carry. *)
    { builder_id = t.id
    ; frame_gen = t.frames.(t.depth - 1).gen
    ; pos = Dynarray.length t.children
    ; stamp = tick t
    }
  else failwith "Builder.checkpoint: no open node"
;;

(* [cp] addresses the children from [cp.pos] onwards. A [start_node_at] at [p]
   replaces everything from [p] with a single node, so it strands [cp] exactly
   when [p < cp.pos] and it ran after [cp] was taken. Both halves matter:

   - one at [p >= cp.pos] repackages children [cp] already addressed, and [cp]
     goes on addressing them, which is what makes reusing a single checkpoint
     down a left-associative chain work;
   - one that ran *before* [cp] was taken cannot have moved anything [cp] points
     at, because [cp] recorded the buffer as that call left it.

   Positions decrease towards the tail, so the walk stops at the first entry
   older than [cp]: everything past it is older still. *)
let rec stranded start_ats (cp : checkpoint) =
  match start_ats with
  | [] -> false
  | sa :: older -> sa.sa_stamp > cp.stamp && (sa.sa_pos < cp.pos || stranded older cp)
;;

(* [sa] subsumes every earlier entry at or right of [sa.sa_pos]: it is newer
   than any checkpoint those could answer for, and it strands everything they
   stranded. Dropping them keeps the list short (one entry under the
   left-associative idiom, empty for a frame [start_node_at] never touched) and
   keeps its positions decreasing, which is what [stranded] walks. *)
let record_start_at top (sa : start_at) =
  let rec prune = function
    | earlier :: older when earlier.sa_pos >= sa.sa_pos -> prune older
    | kept -> kept
  in
  top.start_ats <- sa :: prune top.start_ats
;;

let start_node_at t ?(payload = 0) (cp : checkpoint) kind =
  if cp.builder_id <> t.id
  then failwith "Builder.start_node_at: checkpoint is from another builder";
  if t.depth = 0 then failwith "Builder.start_node_at: no open node";
  let top = t.frames.(t.depth - 1) in
  if top.gen <> cp.frame_gen
  then failwith "Builder.start_node_at: checkpoint is not from the open frame";
  if stranded top.start_ats cp
  then
    (* An earlier [start_node_at] left of [cp.pos] ran after [cp] was taken, so
       the children [cp] addressed are inside that node now and [cp.pos]
       addresses whatever has landed there since. Rejected, in place of wrapping
       the wrong span. *)
    failwith "Builder.start_node_at: checkpoint position is stale";
  if cp.pos > Dynarray.length t.children
  then
    (* Subsumed by the check above: the buffer shrinks only through
       [start_node_at], and a call that leaves it shorter than [cp.pos] started
       left of [cp.pos] and so stranded [cp]. Kept as a backstop, holding
       [children_start] inside the buffer if that bookkeeping ever goes
       wrong. *)
    failwith "Builder.start_node_at: checkpoint position is stale";
  (* The children to be wrapped already sit at [cp.pos] onwards, so the new
     frame claims them just by starting there. The outer frame keeps its own
     [children_start] and picks up again once this one closes. *)
  record_start_at top { sa_pos = cp.pos; sa_stamp = tick t };
  push_frame t ~kind ~payload ~children_start:cp.pos
;;
