# siesta

[![CI](https://github.com/enetsee/siesta/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/enetsee/siesta/actions/workflows/ci.yml)
[![Docs](https://github.com/enetsee/siesta/actions/workflows/docs.yml/badge.svg?branch=main)](https://github.com/enetsee/siesta/actions/workflows/docs.yml)

A **language-independent concrete-syntax-tree (CST) library** for OCaml.

`siesta` provides the immutable, lossless, hash-consed tree substrate that a
parser, editor, or refactoring tool builds on: the *red-green tree* design
made popular by Roslyn and adopted by rust-analyzer's [rowan] / [cstree]. It
knows nothing about any particular grammar: node and token *kinds* are plain
integers you assign.

[API documentation](https://enetsee.github.io/siesta/siesta/Siesta/index.html)

[rowan]: https://github.com/rust-analyzer/rowan
[cstree]: https://github.com/domenicquirl/cstree

```
                          ┌────────────────────────────────────────────┐
   your parser  ─events─▶ │  Builder   event-driven green construction │
                          └───────────────────┬────────────────────────┘
                                              │ mk_node / mk_token
                          ┌───────────────────▼────────────────────────┐
                          │  Green    immutable, hash-consed, O(1)     │
                          │           accessors, position-independent  │
                          └───────────────────┬────────────────────────┘
                                              │ interned via
                          ┌───────────────────▼────────────────────────┐
                          │             Cache / Dedup                  │
                          │             weak hash-cons                 │
                          └────────────────────────────────────────────┘
                          ┌────────────────────────────────────────────┐
   navigation & edits ◀── │  Syntax   persistent "red" cursor layer    │
                          │           parent pointers, replace, splice │
                          └────────────────────────────────────────────┘
```

---

## Why red-green trees

A CST is *lossless*: it keeps every byte of the source (whitespace, comments,
even the shape of syntax errors) so tooling can round-trip text exactly and
point diagnostics back at precise ranges. The classic tension is that lossless
trees are large and edits should be cheap. Red-green trees resolve it with two
layers:

- **Green** nodes are immutable, position-*independent* values. They store a
  `kind`, their children, and a cached `text_len` but *not* their absolute
  offset. That makes a subtree shareable: `1 + 1` interns the two `1` tokens to
  a single value. `siesta` goes further and **hash-conses** every green value, so
  structurally identical subtrees anywhere in any tree (built through the same
  cache) are literally the same OCaml value.

- **Red** cursors (`Syntax.t`) are the position-*dependent* view. A cursor
  bundles a green pointer with its parent, absolute offset, and index. Cursors
  are created lazily on navigation and memoized.

The payoff:

- **O(1) "did this subtree change?"**. `Green.equal` is an integer `tag`
  comparison. After an incremental re-parse through a shared cache, unchanged
  subtrees compare equal by tag with no structural walk.
- **Persistent edits**. `Syntax.replace` / `splice_children` return a new tree
  sharing every off-spine subtree with the old one by physical identity. Only
  the O(depth) spine from root to the edit is rebuilt.
- **Content-keyed memoization**. `Green.tag` is a stable identity that is never
  reissued, so an analysis depending only on a subtree can be memoized in a
  table of your own keyed on the tag. In `((1+2)+(3+4)) + ((1+2)+(3+4))` the two
  halves are one node, so that is one entry and one lookup.

---

## Module tour

The public surface is exactly the five modules re-exported from `Siesta`
(`lib/siesta.mli`). Everything else is library-private.

| Module | Role |
| --- | --- |
| `Green` | Immutable, hash-consed tree: nodes, tokens, children. All accessors O(1). |
| `Builder` | Event-driven green construction (`start_node` / `token` / `finish_node`) with checkpoints. |
| `Syntax` | Persistent red-cursor layer: navigation, parent pointers, offset lookup, traversal, `Ptr`, `replace` / `splice`. |
| `Cache` | Hash-cons cache. `Hashconsed` (sharing), `Plain` (one-shot, no dedup), or `Synchronized` (shared between domains). |
| `Dedup` | The specialised weak-table hash-cons engine. Internal, exposed only so the test suite can drive it directly; use `Cache` with `Green` instead. |

### Green: the tree

```ocaml
type token
type node
type child = Node of node | Token of token

val kind         : node -> int          (* your grammar's tag, an arbitrary int *)
val text_len     : node -> int          (* cached; sum of descendant token lengths *)
val payload      : node -> int          (* per-instance metadata; 0 = none *)
val tag          : node -> int          (* hash-cons identity *)
val equal        : node -> node -> bool (* tag comparison *)
val num_children : node -> int
val nth_child    : node -> int -> child option
val to_source    : node -> string       (* concatenate all token text *)
val pp           : Format.formatter -> node -> unit

val mk_token : Cache.t -> kind:int -> text:string -> token
val mk_node  : Cache.t -> kind:int -> ?payload:int -> children:child array -> unit -> node
```

- **`kind`** is whatever integer your grammar assigns. The convention (see the
  calc example) is a `module K` of named `let`s.
- **`payload`** is per-instance metadata stamped at construction. `0` means
  "none" and preserves sharing; any other value participates in hash-cons
  identity, so two otherwise-identical nodes with different payloads are
  distinct cache entries. Meant for sparse use, such as linking an
  error-recovery placeholder back to a diagnostic. Setting it on every node
  defeats sharing.
- **`tag`** is the hash-cons identity, assigned from a global monotonic counter.
  Two structurally-equal nodes share a tag **iff** they were interned in the
  same cache with no intervening `clear`. `tag` is *not* a cross-cache
  structural-equality test.
- **`to_source` and `pp` are iterative** (explicit stack), so they are safe on
  arbitrarily deep trees, with no stack overflow.

### Builder: constructing green trees

A recursive-descent parser emits a flat stream of events in source order:

```ocaml
let b = Builder.create () in
Builder.start_node b bin_expr;
Builder.token b int_lit "1";
Builder.token b plus "+";
Builder.token b int_lit "1";
Builder.finish_node b;
let root = Builder.finish b in
assert (Green.to_source root = "1+1")
```

**Checkpoints** solve the left-associative-operator problem. You don't know
you're inside a `BIN_EXPR` until you've already emitted its left operand and
seen the operator. `checkpoint` snapshots the write position; `start_node_at`
retroactively wraps everything emitted since:

```ocaml
Builder.start_node b root;
let cp = Builder.checkpoint b in
Builder.token b int_lit "1";
Builder.start_node_at b cp bin_expr;   (* wraps the "1" that's already there *)
Builder.token b plus "+";
Builder.token b int_lit "2";
Builder.finish_node b;                 (* → BIN(1, +, 2) *)
```

Reusing the same `cp` for each operator builds `1+2+3` as `BIN(BIN(1,+,2),+,3)`.
Checkpoints are bound to their frame: using one after that frame has closed, or
after a deeper frame was pushed on top, raises `Failure` rather than silently
wrapping the wrong children. So does using one that an earlier checkpoint from
the same frame has since swallowed, which leaves it pointing past the end. The
emitters raise `Failure` on misuse too (wrong nesting, token before any node,
finish with open frames, use after `finish`).

### Syntax: navigating and editing

```ocaml
let cursor = Syntax.of_root green_root in
Syntax.kind cursor;               (* : int *)
Syntax.text_range cursor;         (* : int * int, absolute (lo, hi) *)
Syntax.parent cursor;             (* : t option *)
Syntax.children_array cursor;     (* : elem array, memoized *)
```

Two equalities, deliberately distinct:

- `equal a b`: same offset **and** same green tag. Structural sameness at a
  position; does *not* check the two cursors share a root.
- `same_tree a b`: same root by physical identity (same `of_root` call).

**Persistent mutation** returns `{ root; self }`, the new root cursor and a
fresh cursor at the same path as the target:

```ocaml
val replace         : Cache.t -> t -> Green.node -> t result
val splice_children : Cache.t -> t -> at:int -> remove:int -> Green.child list -> t result
val replace_child   : Cache.t -> elem -> Green.child -> t result   (* cursor-based *)
val splice_at       : Cache.t -> elem -> remove:int -> Green.child list -> t result
```

The old tree is untouched; unchanged subtrees off the root→target spine are
physically shared with the result via hash-consing. `splice_children`'s `at` is
a **raw** child index, so it counts trivia too.

**Offset lookup and traversal**, for editor features that start from a cursor
position and for building an id map over a whole file:

```ocaml
val token_at_offset : t -> int -> token_cursor option
val node_at_offset  : t -> int -> t option        (* innermost containing node *)
val ancestors       : t -> t Seq.t                (* includes self *)

type visit = Descend | Skip
val preorder    : t -> f:(t -> visit) -> unit     (* Skip prunes the subtree *)
val descendants : t -> t Seq.t
```

Containment is half-open, so an offset belongs to the child whose range is
`[lo, hi)` and a boundary position resolves to the right-hand token; a node of
zero width has an empty range, so no offset lands inside it. `preorder` uses an
explicit stack, so deep trees are safe, and `Skip` bounds an id-map build to the
kinds it cares about rather than materialising a cursor for every node.

### Syntax.Ptr: addressing an occurrence

Hash-consing means two identical subtrees are one green value, so `Green.tag`
identifies *content*; a `Ptr` identifies *position*. It is a child-index path
plus the kind found there, holding no reference to the tree.

```ocaml
val of_node   : t -> Ptr.t
val resolve   : t -> Ptr.t -> elem option    (* never raises *)
val equal : Ptr.t -> Ptr.t -> bool
val hash  : Ptr.t -> int                     (* usable as a Hashtbl key *)
val is_ancestor : Ptr.t -> Ptr.t -> bool     (* prefix test, no resolution *)
```

A ptr is good against the tree it came from. An edit that shifts child indices
above it will make it resolve elsewhere, or fail on the recorded kind. Stability
belongs a layer up: map identifiers your grammar defines to `Ptr.t`s, and
rebuild that map each parse so no pointer has to survive an edit. rust-analyzer
does the same, and keys its queries on `AstId`.

### Cache: hash-cons modes

```ocaml
val create              : ?capacity:int -> unit -> t  (* Hashconsed: structural sharing *)
val create_plain        : unit -> t                   (* Plain: fresh alloc every time *)
val create_synchronized : ?capacity:int -> unit -> t  (* Hashconsed behind a mutex *)
val clear               : t -> unit
```

- **Hashconsed**. Weak-table dedup. Use it for incremental editors where a
  re-parse benefits from physical equality with the previous tree.
- **Plain**. Just a monotonic tag counter; every construction allocates fresh
  with no hash probe. For one-shot parses the dedup machinery is pure overhead,
  so Plain trades it away. `Green.equal` still works via tags; cross-tree
  sharing is lost. `bench/bench.ml` measures the two modes side by side on
  high-sharing and low-sharing corpora.
- **Synchronized**. Hashconsed behind a `Mutex.t`, for a cache shared between
  domains. Interning serialises, and that cost buys hash-cons identity holding
  across domains. See [Domains](#domains).

`clear` drops entries but never resets the global tag counter, so previously
issued handles keep unique tags. **Do not call it mid-build**: open frames would
mix pre- and post-clear tags and silently split hash-cons (documented at length
in `siesta.mli`).

---

## Quick start

```
opam install . --deps-only --with-test
dune build
dune test
dune exec examples/calc/calc.exe
```

`siesta` has **no runtime dependencies beyond the OCaml stdlib**. The test suite
uses `alcotest`, `qcheck-core` and `qcheck-alcotest`.

Minimal use:

```ocaml
open Siesta

let () =
  let cache = Cache.create () in
  let b = Builder.create ~cache () in
  Builder.start_node b 10;                 (* 10 = your "bin_expr" kind *)
  Builder.token b 1 "1";
  Builder.token b 2 "+";
  Builder.token b 1 "1";
  Builder.finish_node b;
  let root = Builder.finish b in
  Format.printf "%a@." Green.pp root;
  Printf.printf "source = %S, len = %d\n" (Green.to_source root) (Green.text_len root)
```

---

## Example: `examples/calc/`

A ~970-line toy calculator that touches most of the public API. It
demonstrates:

- a **typed lexer** with its own token variant, translated to `siesta`'s `int`
  kinds at exactly one boundary;
- a **recursive-descent parser** using `Builder` checkpoints for
  left-associative `+ - * /` with correct precedence and parentheses;
- **typed AST views**, rust-analyzer-style newtypes over `Syntax.t` with `cast`
  + typed accessors (`Int_node`, `Bin_expr`, `Paren_expr`, `Expr`);
- **lowering** from the lossless CST to a lossy semantic IR with an explicit
  `Hole` constructor absorbing every shape of syntax error, each carrying the
  source span for later diagnostics;
- a **constant-fold rewrite** as a bottom-up green-tree walker; hash-consing
  keeps untouched subtrees physically shared, and an `any_changed` guard skips
  reallocation where children are unchanged;
- **hover**, going from a byte offset to a token to the enclosing expression
  with `token_at_offset` and `ancestors`;
- **an id map over the literals**, keyed on a stable ordinal and holding
  `Syntax.Ptr` values rebuilt from the tree, the firewall pattern in miniature;
- **four persistent edits** through the red layer (`replace`, `replace_child`,
  `splice_children`, `splice_at`), each leaving the original tree intact and
  sharing everything off the rebuilt spine;
- **both cache modes side by side**, showing the identical halves shared under
  Hashconsed and separate under Plain.

Sample output for `1 + 2 * 3`:

```
  roundtrip ok? true
  shape: 9 bytes, 6 nodes, 1 root children
  first child: K21, 9 bytes, at 0..9
  lowered ast = (+ 1 (* 2 3))
  eval (lowered) = 7
  hover @4: token "2" at 4..5, child 0 of K20, inside "2" at /0/4/0:K20
           innermost K20, child 0 of K21, token tag 5
  fold source = "7"
  cache: 10 nodes, 8 tokens (live)
```

`error` slots and `Hole`s are what keep the pipeline **total**. A partial input
like `1 +` still parses, lowers, and reports a hole rather than throwing.

---

## Testing

115 tests across eight binaries.

| Binary | Tests | Focus |
| --- | --- | --- |
| `test_siesta` | 46 | Unit tests of the public surface: hash-cons identity, subtree sharing, weak-table & clear behaviour, hash distribution, Builder events & errors, checkpoints, deep (10 000-level) trees, red-layer navigation, `replace` / `splice`. |
| `test_nav` | 20 | Offset lookup, traversal and `Syntax.Ptr`: occurrence addressing across one hash-consed green node, ptr round-trip (nodes, tokens, root, 10 000 deep), total resolution under every failure mode, half-open boundaries, `Skip` pruning, `elem` and printer coverage, plus three QCheck2 properties. |
| `test_props` | 18 | Property tests (QCheck2): source round-trip, `text_len`/`text_range` consistency & partitioning, hash-cons determinism, builder-vs-direct equivalence, off-spine sharing under `replace`/`splice`, parent↔child round-trip, idempotent replace, splice arithmetic, payload survival through a spine rebuild, `same_tree`, ptr occurrence separation. Plus a census that fails if the generator stops producing the shapes those properties need. |
| `test_dedup` | 13 | Direct unit tests of the `Dedup` tables: idempotence, distinctness, tag monotonicity (100 K inserts), forced collisions, resize survival, deep chains, 5 000-wide arrays, hash distribution, high-entropy payloads on childless nodes. |
| `test_plain` | 9 | Plain cache mode: distinct tags / no sharing, round-trip vs Hashconsed, stats, clear no-op, red-layer over a Plain tree. |
| `test_gc` | 4 | Weak-table GC interaction: collection actually reaps dropped entries, no crash under churn, dedup holds when refs survive, clear preserves tag uniqueness. |
| `test_builder_script` | 3 | Generated builder event scripts, legal and corrupted, checked against a reference model: the builder has to reject at exactly the event the model rejects, and a legal script has to build what the equivalent direct nesting builds. Plus a census over the script corpus. |
| `test_dedup_diff` | 2 | Differential test: replays randomised op-scripts (1 000 × 200 events, plus 5 × 20 K) against an independent strong-ref reference and asserts identical tag-equivalence classes. The `Dedup` engine's correctness rests on this. |

The property, script and differential tests carry the library's invariants:
physical sharing, off-spine persistence, source round-tripping, and builder
legality. The first two ship a census alongside them, because a property that
never meets the shape it constrains passes green and empty.

Run everything with `dune test`; append `--force` to re-run and see per-binary
summaries.

---

## Domains

Green values are immutable, so a `Green.node` or `Green.token` can be read from
any domain once built. Tags come from an atomic counter, so they stay unique
whichever domain issues them.

Everything else is owned by one domain at a time: a cache from `Cache.create`, a
`Builder.t`, and a cursor tree from `Syntax.of_root`.

That leaves two ways to work in parallel, and neither needs a lock:

- **Read-only analysis.** Share the green root, and give each domain its own
  cursor tree. `Syntax.of_root` is O(1), so that costs one record per domain.
  The memoized children arrays behind `Syntax.children_array` are what make a
  cursor tree single-owner, and a per-domain tree side-steps them entirely.
- **Parallel one-shot parses.** `Cache.create_plain` holds no tables, so it is
  shareable. This is already the recommended mode for a one-shot parse, which is
  what a batch of files across domains is.

`Cache.create_synchronized` is the remaining case, for when domains must agree
on hash-cons identity. It is opt-in because interning is the hot path:
`Cache.create` should not pay for a lock the single-domain case never needs.

The underlying constraint is that `Weak` arrays are
[memory-safe but not consistent](https://github.com/ocaml-multicore/ocaml-multicore/wiki/Safety-of-Stdlib-under-Multicore-OCaml)
under concurrent update, so an unsynchronised race corrupts table state silently
rather than crashing. The mutex therefore spans the probe *and* the insert, not
just the write.

**Domains need OCaml >= 5.5.0.** On 5.2 to 5.4 the runtime segfaults in
`ephe_mark` when a domain that allocated weak arrays terminates, which any
Hashconsed cache built inside a domain will hit. It is a runtime bug rather than
a siesta one, fixed by [ocaml/ocaml#14722](https://github.com/ocaml/ocaml/pull/14722)
in 5.5.0. The library itself still builds and runs on 5.2; it is only the
multi-domain use above that needs the newer runtime, and the domain tests are
gated accordingly.

---

## Requirements

- **OCaml ≥ 5.2**. `Builder` uses the stdlib `Dynarray`.
- No runtime dependencies beyond the OCaml stdlib. The test suite uses
  `alcotest` and `qcheck-core`.
- **Domains** need **OCaml >= 5.5.0**, for a runtime fix. See
  [below](#domains). Green values are shareable; caches, builders and cursor
  trees are owned by one domain unless stated otherwise.

---

## Layout

```
lib/
  siesta.ml/.mli           public module aggregation + curated signatures
  green.ml/.mli            immutable hash-consed tree
  builder.ml/.mli          event-driven green construction + checkpoints
  syntax.ml/.mli           persistent red-cursor layer
  cache.ml/.mli            hash-cons cache (Hashconsed | Plain)
  dedup.ml/.mli            weak-table hash-cons engine (internal)
examples/
  calc/                    toy calculator exercising the full stack
test/
  test_siesta.ml           unit tests of the public surface
  test_nav.ml              offset lookup, traversal, Syntax.Ptr
  test_props.ml            QCheck2 property tests
  test_dedup.ml            direct Dedup unit tests
  test_plain.ml            Plain cache-mode tests
  test_gc.ml               weak-table GC-interaction tests
  test_builder_script.ml   generated builder scripts vs a reference model
  test_dedup_diff.ml       differential test vs a strong-ref reference
  helpers.ml/.mli          shared test scaffolding
  testable.ml/.mli         Alcotest testables for the tree types
bench/
  bench.ml                 Gc-based Hashconsed-vs-Plain microbenchmark
```

---

## Credits & license

Released under the **MIT license**; see [`LICENSE`](LICENSE).

The `Dedup` engine implements the hash-consing technique described by Conchon &
Filliâtre, *Type-Safe Modular Hash-Consing* (ACM SIGPLAN Workshop on ML, 2006).
