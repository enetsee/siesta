## v0.1.0

- Initial release.
- Safe under domains on OCaml >= 5.5.0: atomic tag counter, shareable `Plain`
  caches, and `Cache.create_synchronized` for a cache shared between domains.
  Earlier runtimes segfault in `ephe_mark`; fixed upstream in 5.5.0.
- `Syntax.num_children` and `Syntax.nth_child_exn`, and `Green.nth_child_exn`
  in the public surface: child access by index that allocates nothing once a
  node's children are materialized.
