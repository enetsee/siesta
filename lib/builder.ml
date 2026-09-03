type checkpoint =
  { frame_gen : int
  ; pos : int
  }

type frame =
  { kind : int
  ; payload : int
  ; gen : int
  ; children_start : int
  ; mutable lowest_wrap : int
    (* Leftmost position this frame has been wrapped at, or [max_int] if it has
       not been. A wrap at [p] swallows [p, len), so it is exactly the
       checkpoints of this frame with [pos > p] that stop addressing what they
       were taken to address. See [start_node_at]. *)
  }

type t =
  { mutable stack : frame list
  ; mutable root : Green.node option
  ; mutable next_gen : int
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
  { stack = []; root = None; next_gen = 0; cache; children }
;;

let cache t = t.cache

let fresh_gen t =
  let g = t.next_gen in
  t.next_gen <- g + 1;
  g
;;

let start_node t ?(payload = 0) kind =
  if Option.is_some t.root then failwith "Builder.start_node: tree already finished";
  let gen = fresh_gen t in
  t.stack
  <- { kind
     ; payload
     ; gen
     ; children_start = Dynarray.length t.children
     ; lowest_wrap = max_int
     }
     :: t.stack
;;

let token t kind text =
  match t.stack with
  | _ :: _ ->
    let tok = Green.mk_token t.cache ~kind ~text in
    Dynarray.add_last t.children (Green.Token tok)
  | [] ->
    if Option.is_some t.root
    then failwith "Builder.token: tree already finished"
    else failwith "Builder.token: no node started"
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
  match t.stack with
  | top :: rest ->
    let children = pop_children_array t top.children_start in
    let node = Green.mk_node t.cache ~kind:top.kind ~payload:top.payload ~children () in
    t.stack <- rest;
    (match rest with
     | _ :: _ -> Dynarray.add_last t.children (Green.Node node)
     | [] -> t.root <- Some node)
  | [] ->
    if Option.is_some t.root
    then failwith "Builder.finish_node: tree already finished"
    else failwith "Builder.finish_node: no node started"
;;

let finish t =
  match t.stack with
  | [] ->
    (match t.root with
     | Some r -> r
     | None -> failwith "Builder.finish: nothing built")
  | _ :: _ -> failwith "Builder.finish: unbalanced start/finish"
;;

(* -- checkpoints ----------------------------------------------------------- *)

let checkpoint t =
  match t.stack with
  | top :: _ ->
    (* [pos] is the same [children_start] a frame opened here would carry. *)
    { frame_gen = top.gen; pos = Dynarray.length t.children }
  | [] -> failwith "Builder.checkpoint: no open node"
;;

let start_node_at t ?(payload = 0) (cp : checkpoint) kind =
  match t.stack with
  | [] -> failwith "Builder.start_node_at: no open node"
  | top :: _ when top.gen <> cp.frame_gen ->
    failwith "Builder.start_node_at: checkpoint is not from the open frame"
  | top :: _ when cp.pos > top.lowest_wrap ->
    (* An earlier checkpoint of this frame has been wrapped at [lowest_wrap],
       which swallowed everything from there on. Whatever sits at [cp.pos] now
       is not what [cp] was taken to address, so reuse is rejected rather than
       silently wrapping the wrong span. *)
    failwith "Builder.start_node_at: checkpoint position is stale"
  | _ :: _ when cp.pos > Dynarray.length t.children ->
    (* Subsumed by the check above, since the buffer only ever shrinks through a
       wrap. Kept as a cheap backstop: it is what keeps [children_start] inside
       the buffer if the bookkeeping above is ever wrong. *)
    failwith "Builder.start_node_at: checkpoint position is stale"
  | top :: _ ->
    (* The children to be wrapped already sit at [cp.pos] onwards, so the new
       frame claims them just by starting there. The outer frame keeps its
       own [children_start] and picks up again once this one closes. *)
    if cp.pos < top.lowest_wrap then top.lowest_wrap <- cp.pos;
    let gen = fresh_gen t in
    t.stack
    <- { kind; payload; gen; children_start = cp.pos; lowest_wrap = max_int } :: t.stack
;;
