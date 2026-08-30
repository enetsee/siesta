(* Domain-interaction tests.

   Every assertion runs on the main domain after [Domain.join]. Alcotest keeps
   its own mutable state, so calling it from a spawned domain would be a race in
   the test harness rather than in siesta.

   A green run proves less here than elsewhere: a race that does not interleave
   badly still passes. Work is sized to give the scheduler something to
   interleave without slowing CI. *)

open Siesta

(* At least two, or nothing contends. No more than the machine has, or the
   domains queue rather than race. *)
let domains = max 2 (min 4 (Domain.recommended_domain_count ()))
let per_domain = 5_000

(* Run [f i] on [domains] fresh domains and collect the results in order. *)
let parallel f =
  List.init domains (fun i -> Domain.spawn (fun () -> f i)) |> List.map Domain.join
;;

(* A barrier over [n] domains, reusable across rounds.

   Without one the domains do not overlap: each runs its whole workload while
   another is still starting, so they take turns rather than race. Interning
   only corrupts a table when two domains miss on the same key at the same
   moment, and without a barrier these tests passed 30 runs with the mutex
   removed, which made them worthless.

   A spin version of this segfaulted about one run in ten, hence the condition
   variable. *)
let make_barrier n =
  let m = Mutex.create () in
  let cond = Condition.create () in
  let arrived = ref 0 in
  let round = ref 0 in
  fun () ->
    Mutex.protect m (fun () ->
      let r = !round in
      incr arrived;
      if !arrived = n
      then (
        arrived := 0;
        incr round;
        Condition.broadcast cond)
      else
        while !round = r do
          Condition.wait cond m
        done)
;;

(* -- the tag counter ------------------------------------------------------- *)

(* The direct regression test for [Dedup.fresh_tag]. Each domain interns into
   its own cache, so the tables never meet and the counter is the only thing
   shared. [incr] loses updates here and two domains come away holding the same
   tag. *)
let test_tags_unique_across_domains () =
  let all =
    parallel (fun d ->
      let cache = Cache.create () in
      Array.init per_domain (fun i ->
        Green.Token.tag (Green.mk_token cache ~kind:0 ~text:(Printf.sprintf "d%d_%d" d i))))
  in
  let seen = Hashtbl.create (domains * per_domain) in
  List.iter
    (Array.iter (fun tag ->
       if Hashtbl.mem seen tag then Alcotest.failf "tag %d issued twice" tag;
       Hashtbl.add seen tag ()))
    all;
  Alcotest.(check int)
    (Printf.sprintf "%d domains x %d interns, all tags distinct" domains per_domain)
    (domains * per_domain)
    (Hashtbl.length seen)
;;

(* Plain mode holds no tables, so the counter is the whole of its shared state.
   One cache, every domain, still no repeated tag. *)
let test_plain_cache_is_shareable () =
  let cache = Cache.create_plain () in
  let all =
    parallel (fun d ->
      Array.init per_domain (fun i ->
        Green.Token.tag (Green.mk_token cache ~kind:1 ~text:(string_of_int (d + i)))))
  in
  let seen = Hashtbl.create (domains * per_domain) in
  List.iter
    (Array.iter (fun tag ->
       if Hashtbl.mem seen tag then Alcotest.failf "tag %d issued twice" tag;
       Hashtbl.add seen tag ()))
    all;
  Alcotest.(check int)
    "shared Plain cache: every intern got its own tag"
    (domains * per_domain)
    (Hashtbl.length seen)
;;

(* -- the synchronized cache ------------------------------------------------ *)

(* Every domain interns the same vocabulary into one synchronized cache and
   keeps the tokens it got. Dedup held iff every domain came away with the same
   record for a given word; "it did not crash" is weaker and passes even
   unlocked, since weak arrays are memory-safe.

   The barrier holds every domain at the top of each round, and each round uses
   a vocabulary it has never seen, so every intern in it is a miss. Misses are
   the only interns that write, so that is where two domains can both mint a
   record for one word.

   The domains return the tokens, not their tags. Tags alone would leave nothing
   referencing the interned records, so the weak table could reap an entry
   between one domain interning a word and the next reaching it; the re-intern
   mints a fresh tag and the comparison fails on collection rather than on
   locking. test_gc.ml covers that behaviour. *)
let test_synchronized_cache_dedups () =
  let rounds = 150
  and per_round = 40 in
  let cache = Cache.create_synchronized () in
  let barrier = make_barrier domains in
  let word r w = Printf.sprintf "r%d_w%d" r w in
  let all =
    parallel (fun _ ->
      let got = Array.make (rounds * per_round) None in
      for r = 0 to rounds - 1 do
        barrier ();
        for w = 0 to per_round - 1 do
          got.((r * per_round) + w)
          <- Some (Green.mk_token cache ~kind:2 ~text:(word r w))
        done
      done;
      Array.map Option.get got)
  in
  match all with
  | [] -> Alcotest.fail "no domains ran"
  | first :: rest ->
    List.iteri
      (fun d toks ->
         Array.iteri
           (fun i tok ->
              if not (tok == first.(i))
              then
                Alcotest.failf
                  "domain %d got a separate record for %S (tag %d vs %d)"
                  (d + 1)
                  (word (i / per_round) (i mod per_round))
                  (Green.Token.tag tok)
                  (Green.Token.tag first.(i)))
           toks)
      rest;
    Alcotest.(check int)
      "one record per distinct word, shared by every domain"
      (rounds * per_round)
      (List.length
         (List.sort_uniq compare (List.map Green.Token.tag (Array.to_list first))))
;;

(* Nodes as well as tokens, so [node_intern]'s probe contends too. Same barrier
   and same fresh-per-round vocabulary as above, for the same reason. *)
let test_synchronized_cache_nodes () =
  let rounds = 150
  and per_round = 20 in
  let cache = Cache.create_synchronized () in
  let barrier = make_barrier domains in
  let all =
    parallel (fun _ ->
      let got = Array.make (rounds * per_round) None in
      for r = 0 to rounds - 1 do
        barrier ();
        for w = 0 to per_round - 1 do
          let tok = Green.mk_token cache ~kind:3 ~text:(Printf.sprintf "n%d_%d" r w) in
          got.((r * per_round) + w)
          <- Some (Green.mk_node cache ~kind:30 ~children:[| Green.Token tok |] ())
        done
      done;
      Array.map Option.get got)
  in
  match all with
  | [] -> Alcotest.fail "no domains ran"
  | first :: rest ->
    List.iter
      (fun nodes ->
         Array.iteri
           (fun i n ->
              if not (n == first.(i))
              then Alcotest.failf "node %d was interned twice over" i)
           nodes)
      rest;
    let s = Cache.node_stats cache in
    Alcotest.(check int) "every node interned once" (rounds * per_round) Cache.(s.entries);
    (* Keep them alive past the stats read, or the count above races the GC. *)
    ignore (Sys.opaque_identity all)
;;

(* -- green values are shareable -------------------------------------------- *)

(* One tree, built once, read from every domain. Each domain makes its own
   cursor tree with [of_root]: the green root is shared, the memoized children
   arrays are not. *)
let test_shared_green_read_from_many_domains () =
  let cache = Cache.create () in
  let b = Builder.create ~cache () in
  Builder.start_node b 100;
  for i = 0 to 199 do
    Builder.start_node b 101;
    Builder.token b 1 (string_of_int i);
    Builder.token b 2 "+";
    Builder.finish_node b
  done;
  Builder.finish_node b;
  let root = Builder.finish b in
  let expected_src = Green.to_source root in
  let expected_nodes = Seq.length (Syntax.descendants (Syntax.of_root root)) in
  let all =
    parallel (fun _ ->
      let cur = Syntax.of_root root in
      Syntax.to_source cur, Seq.length (Syntax.descendants cur))
  in
  List.iteri
    (fun d (src, n) ->
       Alcotest.(check string) (Printf.sprintf "domain %d source" d) expected_src src;
       Alcotest.(check int) (Printf.sprintf "domain %d node count" d) expected_nodes n)
    all
;;

(* -- domain-local caches still build correct trees ------------------------- *)

(* The default path, run in parallel. Same shape from every domain, so each one
   must produce the same source and the same node count, and within a domain the
   cache must still be sharing. *)
let test_domain_local_caches_still_share () =
  let all =
    parallel (fun _ ->
      let cache = Cache.create () in
      let build () =
        let b = Builder.create ~cache () in
        Builder.start_node b 10;
        Builder.token b 1 "1";
        Builder.token b 2 "+";
        Builder.token b 1 "1";
        Builder.finish_node b;
        Builder.finish b
      in
      let a = build () in
      let b' = build () in
      Green.to_source a, a == b')
  in
  List.iteri
    (fun d (src, shared) ->
       Alcotest.(check string) (Printf.sprintf "domain %d source" d) "1+1" src;
       Alcotest.(check bool)
         (Printf.sprintf "domain %d still hash-conses within its own cache" d)
         true
         shared)
    all
;;

let () =
  Alcotest.run
    "multicore"
    [ ( "tag counter"
      , [ Alcotest.test_case
            "tags unique across domains"
            `Quick
            test_tags_unique_across_domains
        ; Alcotest.test_case
            "plain cache is shareable"
            `Quick
            test_plain_cache_is_shareable
        ] )
    ; ( "synchronized cache"
      , [ Alcotest.test_case
            "dedups under contention"
            `Quick
            test_synchronized_cache_dedups
        ; Alcotest.test_case "nodes dedup too" `Quick test_synchronized_cache_nodes
        ] )
    ; ( "shared green"
      , [ Alcotest.test_case
            "read from many domains"
            `Quick
            test_shared_green_read_from_many_domains
        ; Alcotest.test_case
            "domain-local caches still share"
            `Quick
            test_domain_local_caches_still_share
        ] )
    ]
;;
