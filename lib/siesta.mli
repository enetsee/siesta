(** [siesta], a language-independent concrete-syntax-tree library *)

(** Flat green-record types and the weak dedup tables behind them.

    Used internally by {!Cache}, and exposed only so the test suite can drive
    the tables without going through {!Cache} or {!Builder}. This API may change
    without notice; to build trees, use {!Cache} with {!Green.mk_token} and
    {!Green.mk_node}.

    Tags come from one global monotonic counter ({!Dedup.fresh_tag}), shared by
    every cache and by both record types, so a [tag] is unique across separate
    caches too. *)
module Dedup : sig
  type token = Dedup.token =
    { tk_tag : int
    ; tk_kind : int
    ; tk_text : string
    }

  type node = Dedup.node =
    { nd_tag : int
    ; nd_kind : int
    ; nd_text_len : int
    ; nd_children : child array
    ; nd_payload : int
    }

  and child = Dedup.child =
    | Node of node
    | Token of token

  (** {2 Token table} *)

  type token_t = Dedup.token_t

  val token_create : ?capacity:int -> unit -> token_t
  val token_intern : token_t -> kind:int -> text:string -> token
  val token_clear : token_t -> unit

  (** {2 Node table} *)

  type node_t = Dedup.node_t

  val node_create : ?capacity:int -> unit -> node_t

  val node_intern
    :  node_t
    -> kind:int
    -> text_len:int
    -> payload:int
    -> child array
    -> node

  val node_clear : node_t -> unit

  (** {2 Tag dispenser}

      Exposed so Plain mode draws tags from the same global counter as
      Hashconsed mode, keeping tags unique across caches whichever mode each one
      is in. *)

  val fresh_tag : unit -> int

  (** {2 Diagnostics}

      A 6-tuple {!Cache} unpacks into its public {!Cache.stats} record:
      [(bucket_count, live_entries, total_slots, min_bucket, median_bucket,
      max_bucket)]. *)

  val token_stats : token_t -> int * int * int * int * int * int
  val node_stats : node_t -> int * int * int * int * int * int
end

(** Hash-cons cache.

    A cache holds two weak dedup tables, one for tokens and one for nodes
    (Hashconsed mode), or the same pair behind a mutex (Synchronized mode), or
    nothing but the tag counter (Plain mode). {!Builder} and {!Syntax} share
    identical subtrees by physical equality in the first two; Plain gives up the
    sharing to save the dedup work on a one-shot parse.

    The public surface is just enough to allocate, clear, and inspect a cache;
    the internal hashcons primitives stay private to the library.

    {2 Domains}

    Green values are immutable, so a {!Green.node} or {!Green.token} can be read
    from any domain once built. Tags come from an atomic counter, so they stay
    unique whichever domain hands them out.

    Everything else belongs to one domain at a time: a cache from [create], a
    {!Builder.t}, and a cursor tree from {!Syntax.of_root}.

    All of this needs OCaml >= 5.5.0. Earlier runtimes segfault in [ephe_mark]
    once a domain that allocated weak arrays terminates, which any Hashconsed
    cache built inside a domain will hit. That is a runtime bug, fixed upstream
    in 5.5.0; the library still builds and runs single-domain on 5.2.

    Two ways to work in parallel. For read-only analysis, share the green root
    and give each domain its own cursor tree: {!Syntax.of_root} is O(1), so that
    costs a record per domain and needs no synchronisation. For building,
    [create_plain] holds no tables and is safe to share, which suits parallel
    one-shot parses; [create_synchronized] is for when domains must share
    hash-cons identity. *)
module Cache : sig
  type t = Cache.t

  (** Hashconsed mode, with structural sharing over nodes and tokens. Use this
      for an incremental editor, where a re-parse gets physical equality with
      the previous tree and {!Green.equal} is O(1) on unchanged subtrees. *)
  val create : ?capacity:int -> unit -> t

  (** Plain mode. Every intern allocates a fresh record, skipping the hash and
      the bucket walk. Worth it for a one-shot parse, where the tree is built
      once and then read. {!Green.equal} still works, since it compares tags,
      but you lose sharing across trees.

      Holds no tables, so it is safe to share between domains. *)
  val create_plain : unit -> t

  (** Hashconsed mode behind a mutex, for a cache shared between domains. Every
      intern, clear and stats call takes the lock, so interning serialises; that
      cost buys you hash-cons identity holding across domains.

      Only when domains must share identity. Where each domain can own its own
      cache, {!create} is the same thing without the lock, and where the parse
      is one-shot, {!create_plain} is already shareable. *)
  val create_synchronized : ?capacity:int -> unit -> t

  (** Drops every entry from both tables. Plain mode holds no tables, so there
      it does nothing. Later lookups miss and re-insert. Existing
      {!Green.token} and {!Green.node} handles stay valid with their tags
      intact, but they stop sharing by physical equality with anything built
      after the clear.

      {b Do not call mid-build.} If a {!Builder} on this cache has open frames,
      those frames hold children with pre-clear tags while later emissions get
      post-clear tags. The tree comes out well-formed, but structurally-equal
      subtrees either side of the clear end up with different tags, which
      quietly breaks {!Green.equal}. Finish or abandon any in-flight builders
      first.

      On a {!create_synchronized} cache the lock keeps the tables consistent but
      cannot help with any of that: give the cache to one domain for the
      duration of the clear. *)
  val clear : t -> unit

  (** {2 Diagnostics} *)

  type stats = Cache.stats =
    { table_length : int (** Size of the bucket array; [0] in Plain. *)
    ; entries : int (** Live entries; [0] in Plain. *)
    ; sum_bucket_lengths : int
    ; smallest_bucket : int
    ; median_bucket : int
    ; biggest_bucket : int
    }

  val token_stats : t -> stats
  val node_stats : t -> stats
end

(** Green tree: the lossless, hash-consed representation.

    Green values are immutable, and identical subtrees share representation by
    physical equality, and so by [tag]. Every accessor here is O(1), apart from
    [children_array], which copies. *)
module Green : sig
  type token = Green.token
  type node = Green.node

  type child = Green.child =
    | Node of node
    | Token of token

  (** {2 Node accessors} *)

  val kind : node -> int
  val text_len : node -> int

  (** Per-instance metadata stamped on at construction. [0] is the default and
      means none. Any other value takes part in hash-cons identity, so two
      otherwise-identical nodes with different payloads are separate cache
      entries.

      It is for producers that need a stable id on one particular node instance,
      such as an error-recovery layer linking a placeholder back to a diagnostic
      list. Use it sparingly; a payload on every node costs you the sharing. *)
  val payload : node -> int

  (** Hash-cons identity, assigned at intern time from a monotonic counter. Two
      structurally-equal nodes share a [tag] iff they were interned in the same
      {!Cache.t} with no {!Cache.clear} in between, so comparing tags is a
      structural-equality test only within one cache, between clears. *)
  val tag : node -> int

  (** Equality on hash-cons identity. Within one cache, between clears, this is
      structural equality. Across caches, or across a clear, structurally-equal
      nodes compare unequal. See {!tag}. *)
  val equal : node -> node -> bool

  (** {2 Children} *)

  val num_children : node -> int

  (** [nth_child n i] is the [i]th child, or [None] if [i] is out of range. O(1). *)
  val nth_child : node -> int -> child option

  (** [children_array n] is a fresh copy of the children array, safe to
      mutate. *)
  val children_array : node -> child array

  (** {2 Token accessors}

      Operations on [token], mirroring {!Syntax.module-Token}. All O(1). *)

  module Token : sig
    val kind : token -> int
    val text : token -> string

    (** Hash-cons identity, with the same per-cache and pre-clear caveat as
        [tag] on nodes. *)
    val tag : token -> int

    (** Equality on hash-cons identity, carrying the same caveat as [equal] on
        nodes. *)
    val equal : token -> token -> bool
  end

  (** {2 Construction}

      Both go through {!Cache}, so structurally equal values share a tag by
      physical equality. Use these rather than building records by hand. *)

  val mk_token : Cache.t -> kind:int -> text:string -> token

  (** [mk_node cache ~kind ?payload ~children ()] builds a node whose [text_len]
      is computed by summing the lengths of [children]. [?payload] defaults to
      0; see {!payload} for semantics.

      The node takes its own copy of [children], so the array passed in stays
      free to mutate afterwards. Read, modify and rebuild over one buffer from
      {!children_array} is therefore safe to repeat. *)
  val mk_node
    :  Cache.t
    -> kind:int
    -> ?payload:int
    -> children:child array
    -> unit
    -> node

  (** {2 Helpers} *)

  val child_text_len : child -> int
  val sum_text_len : child array -> int

  (** Rebuilds the source text by joining all token texts left to right.
      Iterative, so safe on arbitrarily deep trees. *)
  val to_source : node -> string

  (** Prints the tree for inspection and golden tests. Kinds print as [Kn],
      tokens as [(Kn "text")], nodes as nested vertical boxes. Iterative, so safe
      on deep trees. *)
  val pp : Format.formatter -> node -> unit
end

(** Event-driven green-tree builder.

    Drives a {!Cache} from a stream of [start_node], [token] and [finish_node]
    events emitted in source order. From a recursive-descent parser, use
    [checkpoint] with [start_node_at] to wrap the LHS of a binary operator after
    you have seen the operator. *)
module Builder : sig
  type t = Builder.t

  (** A position in the open node's child list, taken by {!val-checkpoint} for
      use with {!start_node_at}. It carries the builder and the frame it came
      from, and the moment it was taken. Passing it to a different builder, or
      using it once its frame has finished or a deeper frame sits on top of it,
      raises [Failure]. *)
  type checkpoint = Builder.checkpoint

  (** [create ?cache ?initial_children_capacity ()] starts a fresh builder. When
      [?cache] is omitted a new cache is allocated.

      [?initial_children_capacity] (default 64) sizes the children buffer up
      front to save the O(log n) regrows during a parse. For a big parse, pass
      roughly the peak child count across all open frames. Too small and the
      buffer just grows; too large costs O(hint) words up front. *)
  val create : ?cache:Cache.t -> ?initial_children_capacity:int -> unit -> t

  (** The cache this builder interns through, whether passed to {!create} or
      allocated by it. Every mutation entry point takes one ({!Syntax.replace},
      {!Syntax.splice_children}, {!Syntax.replace_child}, {!Syntax.splice_at}),
      so keep this if the tree is going to be edited. A different cache rebuilds
      the spine as fresh records, losing the sharing with the original tree. *)
  val cache : t -> Cache.t

  (** {2 Event emitters}

      These raise [Failure] on misuse: wrong nesting, finish before open, a
      token outside any open node. *)

  (** [start_node ?payload kind] opens a node of the given kind. [?payload]
      defaults to 0; anything else stamps the green node with its own identity.
      See {!Green.payload}. *)
  val start_node : t -> ?payload:int -> int -> unit

  val token : t -> int -> string -> unit
  val finish_node : t -> unit

  (** Pops the root. Raises [Failure] if frames are still open, or if no node was
      ever started. The builder is spent afterwards, and further {!start_node}
      calls fail. *)
  val finish : t -> Green.node

  (** {2 Checkpoints} *)

  (** Takes the current write position in the open node's child list. Raises
      [Failure] if no node is open. *)
  val checkpoint : t -> checkpoint

  (** [start_node_at t cp k] retroactively wraps every child emitted into the
      open frame {i since} [cp] in a fresh node of kind [k]. That node is then
      the open frame; close it with {!finish_node} like any other.

      A checkpoint can be reused. After [start_node_at cp k; ...; finish_node]
      the outer frame is open again and [cp] still points at its original write
      offset, so the next [start_node_at cp k'] wraps everything since,
      including the node the previous pair produced. That is the usual
      left-associative idiom: capture once at the start of the LHS, then wrap
      each time you see an operator.

      Raises [Failure] if [cp] came from a different builder, or from a frame
      that is no longer the open one, whether closed or buried under a deeper
      [start_node].

      Raises too if [cp] has been stranded, which means precisely this: an
      earlier [start_node_at] {i left of} [cp]'s position ran {i after} [cp] was
      taken. That call took everything from its own position onwards, so the
      children [cp] addressed sit inside the node it opened and [cp] addresses
      whatever has landed at its offset since. That holds however many children
      have refilled the buffer past it.

      Both halves of the rule carry weight, and dropping either one is a real
      bug rather than conservatism:

      - a [start_node_at] at or {i right of} [cp]'s position leaves [cp] good.
        It repackages children [cp] already addressed, and [cp] goes on
        addressing them. This is what makes reuse work.
      - a checkpoint taken {i after} a [start_node_at] is good however far left
        that call was, because it recorded the buffer as that call left it. This
        is what makes a flat list of items work, one checkpoint per item:

      {[
        List.iter
          (fun item ->
             let cp = Builder.checkpoint b in
             emit_item b item;
             Builder.start_node_at b cp k_item;
             Builder.finish_node b)
          items
      ]}

      Position alone cannot tell those two apart: a [start_node_at] at [p]
      leaves the buffer [p + 1] long, so a checkpoint it stranded and a
      checkpoint taken straight afterwards both sit at [p + 1]. Reusing one
      checkpoint repeatedly is fine; interleaving two from the same frame works
      innermost-last. *)
  val start_node_at : t -> ?payload:int -> checkpoint -> int -> unit
end

(** Red (cursor) layer.

    A node cursor pairs a green pointer with position info (parent, offset,
    index_in_parent). Navigation creates cursors lazily: a parent's children are
    built on first access and then cached, so navigating twice gives you the same
    cursor and [parent <-> child] round-trips by physical equality.

    Tokens get their own cursor type, [token_cursor], so a rewrite can target one
    token. *)
module Syntax : sig
  type t = Syntax.t

  (** Cursor pointing at a token child. *)
  type token_cursor = Syntax.token_cursor

  type elem = Syntax.elem =
    | Node of t
    | Token of token_cursor

  (** Returned by {!replace} and {!splice_children}. [root] is the new root
      cursor. [self] sits at the same path as the target you passed in, but in
      the new tree. *)
  type 'a result = 'a Syntax.result =
    { root : 'a
    ; self : 'a
    }

  (** {2 Construction} *)

  val of_root : Green.node -> t

  (** {2 Navigation and accessors} *)

  val kind : t -> int
  val green : t -> Green.node
  val parent : t -> t option
  val index_in_parent : t -> int
  val text_range : t -> int * int

  (** [equal a b] holds iff the two cursors share an offset and their green
      pointers compare equal. For whether they come from the same root, use
      {!same_tree}.

      It compares position, not occurrence, and that trips people up.
      Hash-consing collapses two structurally-equal subtrees into one green
      node, so a pair of zero-width siblings of the same kind share a green
      pointer {i and} an offset, and
      [equal] answers [true] for them despite differing {!index_in_parent}. To
      name one particular occurrence, say to key a map, diff two trees, or hold
      a position across an edit, reach for {!module-Ptr}: it carries the path,
      so it tells them apart. *)
  val equal : t -> t -> bool

  (** [same_tree a b] holds iff [a] and [b] share root by physical identity (same
      {!of_root} call). *)
  val same_tree : t -> t -> bool

  (** [children_array t] is a fresh copy of [t]'s children, safe to mutate. The
      cursors inside it are the memoized ones, so they stay physically equal to
      what {!nth_child} and a later [children_array] return; it is the array
      holding them that is copied. *)
  val children_array : t -> elem array

  val nth_child : t -> int -> elem option
  val to_source : t -> string
  val pp : Format.formatter -> t -> unit

  (** {3 Accessors that take a node or a token} *)

  val elem_kind : elem -> int
  val elem_text_range : elem -> int * int

  (** [ancestors t] is [t] itself, then its parent, and so on up to the root.
      Lazy, so stopping early only costs the steps you took. *)
  val ancestors : t -> t Seq.t

  (** {2 Token cursor accessors}

      [Token.parent] refers to the outer cursor {!t}, so these are typed in
      terms of [token_cursor]. OCaml has no tidy way to write that mutual
      reference inside a nested signature. *)

  module Token : sig
    val kind : token_cursor -> int
    val text : token_cursor -> string
    val green : token_cursor -> Green.token
    val parent : token_cursor -> t
    val index_in_parent : token_cursor -> int
    val text_range : token_cursor -> int * int
    val equal : token_cursor -> token_cursor -> bool
  end

  (** {2 Offset lookup}

      Find whatever sits at an absolute byte offset. This is what hover, goto and
      selection start from.

      Containment is half-open, so an offset belongs to the child whose range is
      [\[lo, hi)] and a position on the boundary between two tokens resolves to
      the right-hand one. Offsets outside [\[0, text_len)] give [None], including
      [offset = text_len].

      A node of zero width has an empty range, so no offset lands inside it. *)

  (** [token_at_offset root offset] is the token whose range contains
      [offset]. *)
  val token_at_offset : t -> int -> token_cursor option

  (** [node_at_offset root offset] is the innermost node whose range contains
      [offset]; it is at least [root] whenever [offset] is in range. *)
  val node_at_offset : t -> int -> t option

  (** {2 Downward traversal} *)

  (** Whether {!preorder} should walk into a node's children. *)
  type visit = Syntax.visit =
    | Descend
    | Skip

  (** [preorder t ~f] applies [f] to [t], then to every node descendant in source
      order, skipping the subtree of any node where [f] returns {!Skip}. Only
      nodes are visited.

      Use {!Skip} to keep the walk to the kinds you care about, so it doesn't
      build a cursor for every node in the file.

      Iterative, so arbitrarily deep trees are safe. *)
  val preorder : t -> f:(t -> visit) -> unit

  (** [descendants t] is [t] and every node descendant, in source order,
      lazily. *)
  val descendants : t -> t Seq.t

  (** {2 Pointers}

      A {!Ptr.t} addresses a position in a tree as plain integers: a child-index
      path, plus the kind found there. It holds no reference to the tree, so you
      can store it, compare it, hash it, and resolve it back to a cursor later.

      A ptr is good against the tree you took it from. An edit that shifts child
      indices above it will make it resolve somewhere else, or fail on the
      recorded kind. If you need an address that outlives edits, keep a map from
      your own identifiers to ptrs and rebuild it each parse. *)

  module Ptr : sig
    type cursor := t
    type t = Syntax.Ptr.t

    val of_node : cursor -> t
    val of_token : token_cursor -> t
    val of_elem : elem -> t

    (** [resolve root p] is the element [p] addresses. Gives [None] if the path
        runs out of range, meets a token where it wants a node, or finds a
        different kind at the target. *)
    val resolve : cursor -> t -> elem option

    (** [resolve_node] is {!resolve} for ptrs that address a node. *)
    val resolve_node : cursor -> t -> cursor option

    (** Kind of the addressed element, recorded when the ptr was built. *)
    val kind : t -> int

    (** Steps from the root; [0] for a ptr at the root. *)
    val depth : t -> int

    val equal : t -> t -> bool

    (** Lexicographic on the path, so a ptr sorts immediately before its
        descendants; {!kind} breaks ties. *)
    val compare : t -> t -> int

    val hash : t -> int

    (** [is_ancestor a b] holds iff [a] addresses a strict ancestor of [b]. Just
        compares the paths; neither ptr gets resolved. *)
    val is_ancestor : t -> t -> bool

    (** Renders as [/0/2/1:K7], the child-index path followed by the kind. *)
    val pp : Format.formatter -> t -> unit
  end

  (** {2 Mutation}

      Both {!replace} and {!splice_children} are persistent. The old tree is left
      alone, and hash-consing physically shares every unchanged subtree off the
      spine with the result. *)

  (** [replace cache target new_green] returns a new tree in which the node at
      [target] is replaced by [new_green]. The returned [self] cursor sits at the
      same path in the new tree. *)
  val replace : Cache.t -> t -> Green.node -> t result

  (** [splice_children cache target ~at ~remove inserts] removes [remove]
      children of [target] starting at index [at], inserting [inserts] in their
      place.

      Raises [Invalid_argument] if [at] or [remove] are out of range. [at] is a
      raw index into the children array, so it counts trivia too. *)
  val splice_children
    :  Cache.t
    -> t
    -> at:int
    -> remove:int
    -> Green.child list
    -> t result

  (** {3 Cursor-based variants}

      These wrap {!splice_children}, working [(parent, index)] out from the
      cursor's own back-pointers, which saves you counting raw indices by hand.
      Both raise [Failure] if [child] is a node cursor at the root; use
      {!replace} there. *)

  (** [replace_child cache child new_green] replaces [child] with [new_green] in
      its parent. [result.self] is the rebuilt parent cursor (matching
      {!splice_children}'s convention). *)
  val replace_child : Cache.t -> elem -> Green.child -> t result

  (** [splice_at cache child ~remove inserts] removes [remove] children of
      [child]'s parent starting at [child] (inclusive), inserting [inserts] in
      their place. With [~remove:0] this inserts immediately {b before} [child].
      [result.self] is the rebuilt parent cursor. *)
  val splice_at : Cache.t -> elem -> remove:int -> Green.child list -> t result
end
