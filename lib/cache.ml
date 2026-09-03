(* Hash-cons cache, in three modes.

   [Hashconsed] dedups through [Dedup]'s weak tables, so a re-parse reuses green
   nodes of identical kind, children and payload, and "did this subtree change"
   becomes an O(1) [Green.equal]. That is what an editor re-parsing on every
   keystroke wants.

   [Plain] is a bare tag counter, allocating on every intern. Sharing buys a
   one-shot parse nothing, so it skips the hash and the bucket walk.

   [Synchronized] is [Hashconsed] behind a mutex, for a cache shared between
   domains. The lock spans the probe and the insert together, since interning is
   one compound operation over a weak table. A separate constructor rather than
   a flag, so the single-domain path pays nothing. *)

type t =
  | Hashconsed of
      { tokens : Dedup.token_t
      ; nodes : Dedup.node_t
      }
  | Synchronized of
      { lock : Mutex.t
      ; tokens : Dedup.token_t
      ; nodes : Dedup.node_t
      }
  | Plain

type stats =
  { table_length : int
  ; entries : int
  ; sum_bucket_lengths : int
  ; smallest_bucket : int
  ; median_bucket : int
  ; biggest_bucket : int
  }

let default_capacity = 256

let create ?(capacity = default_capacity) () =
  Hashconsed
    { tokens = Dedup.token_create ~capacity (); nodes = Dedup.node_create ~capacity () }
;;

let create_synchronized ?(capacity = default_capacity) () =
  Synchronized
    { lock = Mutex.create ()
    ; tokens = Dedup.token_create ~capacity ()
    ; nodes = Dedup.node_create ~capacity ()
    }
;;

let create_plain () = Plain

let hashcons_token (t : t) ~kind ~text : Dedup.token =
  match t with
  | Hashconsed { tokens; _ } -> Dedup.token_intern tokens ~kind ~text
  | Synchronized { lock; tokens; _ } ->
    Mutex.protect lock (fun () -> Dedup.token_intern tokens ~kind ~text)
  | Plain -> Dedup.{ tk_tag = fresh_tag (); tk_kind = kind; tk_text = text }
;;

let hashcons_node (t : t) ~kind ~text_len ~payload (children : Dedup.child array)
  : Dedup.node
  =
  match t with
  | Hashconsed { nodes; _ } -> Dedup.node_intern nodes ~kind ~text_len ~payload children
  | Synchronized { lock; nodes; _ } ->
    Mutex.protect lock (fun () ->
      Dedup.node_intern nodes ~kind ~text_len ~payload children)
  | Plain ->
    (* Copy for the same reason as the miss path in [Dedup.node_intern]: the
       caller's array must not stay reachable as a built node's children. Plain
       has no bucket to strand an entry in, but [nd_text_len] would still come
       to disagree with the children it was summed from. *)
    Dedup.
      { nd_tag = fresh_tag ()
      ; nd_kind = kind
      ; nd_text_len = text_len
      ; nd_children = Array.copy children
      ; nd_payload = payload
      }
;;

let clear (t : t) =
  match t with
  | Hashconsed { tokens; nodes } ->
    Dedup.token_clear tokens;
    Dedup.node_clear nodes
  | Synchronized { lock; tokens; nodes } ->
    Mutex.protect lock (fun () ->
      Dedup.token_clear tokens;
      Dedup.node_clear nodes)
  | Plain -> ()
;;

let stats_of_tuple
      ( table_length
      , entries
      , sum_bucket_lengths
      , smallest_bucket
      , median_bucket
      , biggest_bucket )
  =
  { table_length
  ; entries
  ; sum_bucket_lengths
  ; smallest_bucket
  ; median_bucket
  ; biggest_bucket
  }
;;

let empty_stats =
  { table_length = 0
  ; entries = 0
  ; sum_bucket_lengths = 0
  ; smallest_bucket = 0
  ; median_bucket = 0
  ; biggest_bucket = 0
  }
;;

let token_stats (t : t) =
  match t with
  | Hashconsed { tokens; _ } -> stats_of_tuple (Dedup.token_stats tokens)
  | Synchronized { lock; tokens; _ } ->
    Mutex.protect lock (fun () -> stats_of_tuple (Dedup.token_stats tokens))
  | Plain -> empty_stats
;;

let node_stats (t : t) =
  match t with
  | Hashconsed { nodes; _ } -> stats_of_tuple (Dedup.node_stats nodes)
  | Synchronized { lock; nodes; _ } ->
    Mutex.protect lock (fun () -> stats_of_tuple (Dedup.node_stats nodes))
  | Plain -> empty_stats
;;
