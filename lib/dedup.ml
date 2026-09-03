(* Hash-consing tables for siesta's green records.

   A weak hash table that hands every distinct value a stable integer tag, so
   structurally-equal values interned through the same table collapse to one
   shared record. Entries are held weakly, so an interned value goes away once
   nothing outside the table references it.

   The technique is Conchon & Filliatre, "Type-Safe Modular Hash-Consing", ACM
   SIGPLAN Workshop on ML, 2006. *)

(* ---- interned records ---------------------------------------------------- *)

(* Both records live in one module and share field names, hence the [tk_] and
   [nd_] prefixes. *)
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

(* ---- identity: one process-wide tag counter ------------------------------ *)

(* One counter feeds every table and both record types, so a tag is unique for
   the life of the process. [clear] leaves it alone, so a tag is never reissued.
   Callers stashing a tag as an identity key depend on that.

   [incr] loses updates under domains, so two caches hand out the same tag and
   [Green.equal] quietly answers true for unrelated nodes. Only an intern miss
   gets this far, so a re-parse that mostly hits never touches it. *)
let tag_counter = Atomic.make 0

(* [fetch_and_add] returns the previous value, so the [+ 1] keeps 1 as the first
   tag handed out. *)
let fresh_tag () = Atomic.fetch_and_add tag_counter 1 + 1

(* ---- hashing ------------------------------------------------------------- *)

(* Multiplicative bit-mixer: xor in the next word, multiply by the 32-bit
   golden-ratio constant, fold the high bits back down with an xorshift. The
   multiply and fold are what scatter near-consecutive inputs, such as the
   monotonic child tags of a deep tree, across the buckets. Monomorphic and
   allocation-free. *)
let[@inline] mix h x =
  let h = h lxor x * 0x9E3779B1 in
  h lxor (h lsr 29)
;;

(* [mix] only ever carries bits upwards, so the last word it mixes reaches
   [bucket_index] with its high bits dropped. Earlier words get folded again by
   later rounds, so only that final one is exposed. This is splitmix64's
   finalizer, constants reduced mod 2^63 to fit an OCaml int, and it brings
   those bits back down. Only [node_hash] wants it: [token_hash] mixes bytes,
   which have no high bits to lose. *)
let[@inline] final h =
  let h = h lxor (h lsr 30) * 0x3F58476D1CE4E5B9 in
  let h = h lxor (h lsr 27) * 0x14D049BB133111EB in
  h lxor (h lsr 31)
;;

let[@inline] hash_string h s =
  let acc = ref h in
  for i = 0 to String.length s - 1 do
    acc := mix !acc (Char.code (String.unsafe_get s i))
  done;
  !acc
;;

(* A child contributes its tag plus a one-bit node/token discriminator, which
   keeps a Node and a Token apart in both the hash and the equality even if
   their tags were ever to coincide. *)
let[@inline] child_key = function
  | Node n -> n.nd_tag lsl 1
  | Token t -> (t.tk_tag lsl 1) lor 1
;;

let token_hash ~kind ~text = hash_string (mix 0 kind) text

let node_hash ~kind ~text_len ~payload children =
  let acc = ref (mix (mix (mix 0 kind) text_len) payload) in
  for i = 0 to Array.length children - 1 do
    acc := mix !acc (child_key children.(i))
  done;
  (* With children the loop has already avalanched [payload]. Childless, it is
     the last word mixed and needs [final], or payloads carrying their entropy
     in the high bits all pile into one bucket. See the "childless payload high
     bits" case in test_dedup.ml. *)
  if Array.length children = 0 then final !acc else !acc
;;

(* ---- generic weak-bucket table ------------------------------------------- *)

type 'a table =
  { mutable buckets : 'a Weak.t array
  ; mutable size : int
    (* entries added since the last resize; exact live count right after one *)
  }

let min_buckets = 16

(* Resize once the average bucket would hold more than [load] entries. *)
let load = 2

(* Every bucket slot starts out pointing at one shared empty weak array, which
   saves [n] allocations up front. Safe because [bucket_put] replaces an empty
   bucket wholesale. *)
let make_table capacity =
  let n = if capacity < min_buckets then min_buckets else capacity in
  { buckets = Array.make n (Weak.create 0); size = 0 }
;;

let clear_table t =
  t.buckets <- Array.make (Array.length t.buckets) (Weak.create 0);
  t.size <- 0
;;

let bucket_index hkey n = hkey land max_int mod n
let grow_cap cap = min (if cap = 0 then 2 else cap * 2) (Sys.max_array_length - 1)

(* Add [v] to bucket [i], reusing a slot the GC has vacated if there is one and
   growing the bucket if there isn't. *)
let bucket_put (buckets : 'a Weak.t array) i (v : 'a) =
  let b = buckets.(i) in
  let cap = Weak.length b in
  let free = ref (-1) in
  let j = ref 0 in
  while !free < 0 && !j < cap do
    if Weak.check b !j then incr j else free := !j
  done;
  if !free >= 0
  then Weak.set b !free (Some v)
  else (
    let cap' = grow_cap cap in
    (* [grow_cap] clamps, so at the ceiling [cap' = cap] and slot [cap] falls
       outside the new bucket. Reaching it takes 2^54 entries in one bucket, so
       the check records the bound and keeps the write in range. *)
    if cap' <= cap then failwith "Dedup.bucket_put: bucket cannot grow further";
    let b' = Weak.create cap' in
    Weak.blit b 0 b' 0 cap;
    Weak.set b' cap (Some v);
    buckets.(i) <- b')
;;

let table_stats t =
  let buckets = t.buckets in
  let n = Array.length buckets in
  let caps = Array.map Weak.length buckets in
  Array.sort Int.compare caps;
  let total = Array.fold_left ( + ) 0 caps in
  let live = ref 0 in
  Array.iter
    (fun b ->
       for j = 0 to Weak.length b - 1 do
         if Weak.check b j then incr live
       done)
    buckets;
  n, !live, total, caps.(0), caps.(n / 2), caps.(n - 1)
;;

(* Rebuild the bucket array, dropping the dead entries and re-placing the live
   ones. [index_of] is the only part that depends on the element type, so it
   comes in as an argument. *)
let rehash t index_of =
  let old = t.buckets in
  let n = Array.length old in
  (* Collect the survivors first, since [t.size] counts inserts and the live
     count is what decides the width. Held strongly for the rebuild only. *)
  let live = ref [] in
  let count = ref 0 in
  Array.iter
    (fun b ->
       for j = 0 to Weak.length b - 1 do
         match Weak.get b j with
         | Some v ->
           live := v :: !live;
           incr count
         | None -> ()
       done)
    old;
  (* Grow under real load, otherwise rebuild at the same width, which still
     drops the dead entries and reclaims the bucket arrays [bucket_put] grew. *)
  let n' = if !count > load * n then min (n * 2) (Sys.max_array_length - 1) else n in
  let fresh = Array.make n' (Weak.create 0) in
  List.iter (fun v -> bucket_put fresh (index_of v n') v) !live;
  t.buckets <- fresh;
  t.size <- !count
;;

(* ---- token table --------------------------------------------------------- *)

type token_t = token table

let token_create ?(capacity = 256) () : token_t = make_table capacity
let token_clear (t : token_t) = clear_table t
let token_stats (t : token_t) = table_stats t

let token_intern (t : token_t) ~kind ~text : token =
  let n = Array.length t.buckets in
  let i = bucket_index (token_hash ~kind ~text) n in
  let b = t.buckets.(i) in
  let cap = Weak.length b in
  let found = ref None in
  let j = ref 0 in
  while Option.is_none !found && !j < cap do
    (match Weak.get b !j with
     | Some e when e.tk_kind = kind && String.equal e.tk_text text -> found := Some e
     | _ -> ());
    incr j
  done;
  match !found with
  | Some e -> e
  | None ->
    let e = { tk_tag = fresh_tag (); tk_kind = kind; tk_text = text } in
    bucket_put t.buckets i e;
    t.size <- t.size + 1;
    if t.size > load * n
    then
      rehash t (fun (v : token) m ->
        bucket_index (token_hash ~kind:v.tk_kind ~text:v.tk_text) m);
    e
;;

(* ---- node table ---------------------------------------------------------- *)

type node_t = node table

let node_create ?(capacity = 256) () : node_t = make_table capacity
let node_clear (t : node_t) = clear_table t
let node_stats (t : node_t) = table_stats t

(* Structural child equality, by tag and discriminator; see [child_key]. *)
let same_children (a : child array) (b : child array) =
  let n = Array.length a in
  Array.length b = n
  &&
  let ok = ref true in
  let i = ref 0 in
  while !ok && !i < n do
    if child_key a.(!i) <> child_key b.(!i) then ok := false;
    incr i
  done;
  !ok
;;

let node_matches (e : node) ~kind ~text_len ~payload children =
  e.nd_kind = kind
  && e.nd_text_len = text_len
  && e.nd_payload = payload
  && same_children e.nd_children children
;;

let node_intern (t : node_t) ~kind ~text_len ~payload (children : child array) : node =
  let n = Array.length t.buckets in
  let i = bucket_index (node_hash ~kind ~text_len ~payload children) n in
  let b = t.buckets.(i) in
  let cap = Weak.length b in
  let found = ref None in
  let j = ref 0 in
  while Option.is_none !found && !j < cap do
    (match Weak.get b !j with
     | Some e when node_matches e ~kind ~text_len ~payload children -> found := Some e
     | _ -> ());
    incr j
  done;
  match !found with
  | Some e -> e
  | None ->
    (* Copy, or the caller keeps a handle on an interned node's children and a
       later write to it corrupts the node silently; [nd_text_len] stops
       matching the children, and the entry sits in a bucket its hash no longer
       indexes, so the shape is lost to every future intern. The cost falls on
       the miss path, which is allocating a record anyway. *)
    let e =
      { nd_tag = fresh_tag ()
      ; nd_kind = kind
      ; nd_text_len = text_len
      ; nd_children = Array.copy children
      ; nd_payload = payload
      }
    in
    bucket_put t.buckets i e;
    t.size <- t.size + 1;
    if t.size > load * n
    then
      rehash t (fun (v : node) m ->
        bucket_index
          (node_hash
             ~kind:v.nd_kind
             ~text_len:v.nd_text_len
             ~payload:v.nd_payload
             v.nd_children)
          m);
    e
;;
