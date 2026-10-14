## v0.1.0

- Initial release.
- Safe under domains on OCaml >= 5.5.0: atomic tag counter, shareable `Plain`
  caches, and `Cache.create_synchronized` for a cache shared between domains.
  Earlier runtimes segfault in `ephe_mark`; fixed upstream in 5.5.0.
- `Syntax.num_children` and `Syntax.nth_child_exn`, and `Green.nth_child_exn`
  in the public surface: child access by index that allocates nothing once a
  node's children are materialized.
- Less minor allocation on the build and navigation paths, with no change to
  behaviour. Building through a warm cache costs 8.5 words per token and node,
  down from 35.4 with a `Synchronized` cache and 21.4 with a `Hashconsed` one:
  - `Synchronized` caches lock directly instead of allocating a
    `Mutex.protect` closure per intern;
  - `Builder` reuses its frame records instead of allocating one per node;
  - `Builder` interns a node's children straight from its buffer, building
    the children array only when the node is new.

  Materializing a node's children costs 9.0 words per child, down from 11.5.
