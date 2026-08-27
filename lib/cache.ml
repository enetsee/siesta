(* Hash-cons cache, in two modes.

   [Hashconsed] dedups through [Dedup]'s weak tables, so a re-parse reuses green
   nodes of identical kind, children and payload, and "did this subtree change"
   becomes an O(1) [Green.equal]. That is what an editor re-parsing on every
   keystroke wants.

   [Plain] is a bare tag counter, allocating on every intern. Sharing buys a
   one-shot parse nothing, so it skips the hash and the bucket walk. *)

type t =
  | Hashconsed of
      { tokens : Dedup.token_t
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

let create_plain () = Plain

let hashcons_token (t : t) ~kind ~text : Dedup.token =
  match t with
  | Hashconsed { tokens; _ } -> Dedup.token_intern tokens ~kind ~text
  | Plain -> Dedup.{ tk_tag = fresh_tag (); tk_kind = kind; tk_text = text }
;;

let hashcons_node (t : t) ~kind ~text_len ~payload (children : Dedup.child array)
  : Dedup.node
  =
  match t with
  | Hashconsed { nodes; _ } -> Dedup.node_intern nodes ~kind ~text_len ~payload children
  | Plain ->
    Dedup.
      { nd_tag = fresh_tag ()
      ; nd_kind = kind
      ; nd_text_len = text_len
      ; nd_children = children
      ; nd_payload = payload
      }
;;

let clear (t : t) =
  match t with
  | Hashconsed { tokens; nodes } ->
    Dedup.token_clear tokens;
    Dedup.node_clear nodes
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
  | Plain -> empty_stats
;;

let node_stats (t : t) =
  match t with
  | Hashconsed { nodes; _ } -> stats_of_tuple (Dedup.node_stats nodes)
  | Plain -> empty_stats
;;
