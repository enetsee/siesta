(* Micro-benchmark harness for Dedup / Cache.

   Allocation and footprint are exact and deterministic. [alloc] is the
   [Gc.allocated_bytes] delta around a workload, garbage included, so it reads
   as GC pressure. [foot] is the [Gc.stat().live_words] delta while the result
   is held alive, so it reads as retained data.

   Time is the noisy one, reported as the median of a few reps after a warm-up,
   inside the same run so the comparison holds.

   Corpora come from a fixed seed, so runs are comparable and both cache modes
   see byte-identical inputs.

   Run: dune exec bench/bench.exe
        dune exec bench/bench.exe -- --scale   (adds the scale sweep) *)

open Siesta

let word_bytes = Sys.word_size / 8

(* -- metric primitives ----------------------------------------------------- *)

let alloc_bytes f =
  let a0 = Gc.allocated_bytes () in
  let r = f () in
  let a1 = Gc.allocated_bytes () in
  r, a1 -. a0
;;

let time_median ~reps f =
  ignore (Sys.opaque_identity (f ()));
  (* warm-up *)
  let ts =
    Array.init reps (fun _ ->
      let t0 = Sys.time () in
      ignore (Sys.opaque_identity (f ()));
      Sys.time () -. t0)
  in
  Array.sort compare ts;
  ts.(reps / 2)
;;

(* Live major-heap words retained by whatever [f] returns, held alive across
   both measurements. [Gc.stat] forces a full major and computes the live set,
   so this is retained data. *)
let retained_words f =
  Gc.full_major ();
  let l0 = Gc.((stat ()).live_words) in
  let keep = f () in
  let l1 = Gc.((stat ()).live_words) in
  ignore (Sys.opaque_identity keep);
  l1 - l0
;;

(* -- shapes + builders ----------------------------------------------------- *)

type shape =
  | Leaf of int * string
  | Branch of int * shape list

module K = struct
  let lit = 1
  let op = 2
  let bin = 10
  let root = 20
end

let build_shape b shape =
  let rec go = function
    | Leaf (k, t) -> Builder.token b k t
    | Branch (k, cs) ->
      Builder.start_node b k;
      List.iter go cs;
      Builder.finish_node b
  in
  go shape
;;

let build_corpus make_cache (corpus : shape array) =
  let cache = make_cache () in
  let roots =
    Array.map
      (fun s ->
         let b = Builder.create ~cache () in
         Builder.start_node b K.root;
         build_shape b s;
         Builder.finish_node b;
         Builder.finish b)
      corpus
  in
  cache, roots
;;

let hashconsed () = Cache.create ()
let plain () = Cache.create_plain ()
let modes = [ "Hashconsed", hashconsed; "Plain", plain ]

(* A small random expression. [lit] turns a drawn int into literal text, and is
   what flips a corpus between high and low sharing while the tree shape stays
   identical, since either version consumes one rng int per leaf. Branching
   below the critical rate, a leaf 3 times in 5, keeps the expressions small and
   program-like. *)
let gen_expr rng ~lit ~max_depth =
  let rec go depth =
    let r = Random.State.int rng 1_000_000 in
    if depth <= 0 || r mod 5 < 3
    then Leaf (K.lit, lit r)
    else Branch (K.bin, [ go (depth - 1); Leaf (K.op, "+"); go (depth - 1) ])
  in
  go max_depth
;;

let corpus ~n ~max_depth ~lit =
  let rng = Random.State.make [| 0x5eed |] in
  Array.init n (fun _ -> gen_expr rng ~lit ~max_depth)
;;

let shared_lit r = string_of_int (r mod 32)

let make_unique_lit () =
  let c = ref 0 in
  fun _ ->
    incr c;
    string_of_int !c
;;

(* -- reporting ------------------------------------------------------------- *)

let header cols =
  Printf.printf "%-12s %10s %10s %11s %11s\n" "mode" cols.(0) cols.(1) cols.(2) cols.(3)
;;

let bench_corpus label (corpus : shape array) =
  Printf.printf "\n## %s  (%d roots)\n" label (Array.length corpus);
  header [| "time(ms)"; "alloc(MB)"; "foot(MB)"; "live-nodes" |];
  List.iter
    (fun (mname, make) ->
       let tmed = time_median ~reps:3 (fun () -> build_corpus make corpus) in
       let _, abytes = alloc_bytes (fun () -> build_corpus make corpus) in
       let fwords = retained_words (fun () -> build_corpus make corpus) in
       let c, roots = build_corpus make corpus in
       ignore (Sys.opaque_identity roots);
       let live = Cache.((node_stats c).entries) in
       Printf.printf
         "%-12s %10.2f %10.2f %11.2f %11d\n"
         mname
         (tmed *. 1000.)
         (abytes /. 1e6)
         (float_of_int (fwords * word_bytes) /. 1e6)
         live)
    modes
;;

let bench_wide () =
  let w = 50_000 in
  let build make =
    let cache = make () in
    let b = Builder.create ~cache () in
    Builder.start_node b K.root;
    for i = 0 to w - 1 do
      Builder.token b K.lit (string_of_int (i mod 64))
    done;
    Builder.finish_node b;
    cache, Builder.finish b
  in
  Printf.printf "\n## wide node (%d children)\n" w;
  header [| "time(ms)"; "alloc(MB)"; "foot(MB)"; "" |];
  List.iter
    (fun (mname, make) ->
       let tmed = time_median ~reps:3 (fun () -> build make) in
       let _, abytes = alloc_bytes (fun () -> build make) in
       let fwords = retained_words (fun () -> build make) in
       Printf.printf
         "%-12s %10.2f %10.2f %11.2f %11s\n"
         mname
         (tmed *. 1000.)
         (abytes /. 1e6)
         (float_of_int (fwords * word_bytes) /. 1e6)
         "")
    modes
;;

let bench_deep () =
  let d = 50_000 in
  let build make =
    let cache = make () in
    let b = Builder.create ~cache () in
    for _ = 1 to d do
      Builder.start_node b K.bin
    done;
    Builder.token b K.lit "x";
    for _ = 1 to d do
      Builder.finish_node b
    done;
    cache, Builder.finish b
  in
  Printf.printf "\n## deep chain (%d nested)\n" d;
  header [| "time(ms)"; "alloc(MB)"; "foot(MB)"; "" |];
  List.iter
    (fun (mname, make) ->
       let tmed = time_median ~reps:3 (fun () -> build make) in
       let _, abytes = alloc_bytes (fun () -> build make) in
       let fwords = retained_words (fun () -> build make) in
       Printf.printf
         "%-12s %10.2f %10.2f %11.2f %11s\n"
         mname
         (tmed *. 1000.)
         (abytes /. 1e6)
         (float_of_int (fwords * word_bytes) /. 1e6)
         "")
    modes
;;

(* Persistent-edit churn: build a base tree, then thread [iters] single-subtree
   replacements through it, dropping each old root. *)
let bench_churn () =
  let iters = 20_000 in
  Printf.printf "\n## churn (%d persistent edits)\n" iters;
  Printf.printf "%-12s %10s %18s\n" "mode" "time(ms)" "live-cache-nodes";
  List.iter
    (fun (mname, make) ->
       let cache = make () in
       let base =
         let b = Builder.create ~cache () in
         Builder.start_node b K.root;
         for i = 0 to 199 do
           Builder.start_node b K.bin;
           Builder.token b K.lit (string_of_int i);
           Builder.token b K.op "+";
           Builder.token b K.lit (string_of_int (i + 1));
           Builder.finish_node b
         done;
         Builder.finish_node b;
         Builder.finish b
       in
       let run () =
         let root = ref (Syntax.of_root base) in
         for i = 0 to iters - 1 do
           let child0 =
             match Syntax.nth_child !root 0 with
             | Some (Syntax.Node c) -> c
             | Some (Syntax.Token _) | None -> assert false
           in
           let repl =
             let b = Builder.create ~cache () in
             Builder.start_node b K.bin;
             Builder.token b K.lit (string_of_int i);
             Builder.token b K.op "+";
             Builder.token b K.lit (string_of_int i);
             Builder.finish_node b;
             Builder.finish b
           in
           let res = Syntax.replace cache child0 repl in
           root := res.Syntax.root;
           if i land 0x3ff = 0 then Gc.minor ()
         done;
         !root
       in
       let tmed = time_median ~reps:3 (fun () -> run ()) in
       (* Hold the final tree alive while reading the cache, so [live] shows the
          steady state of one tree's worth of nodes. The point is that churn
          does not grow it with the edit count. *)
       let last = run () in
       Gc.full_major ();
       let live = Cache.((node_stats cache).entries) in
       ignore (Sys.opaque_identity last);
       Printf.printf "%-12s %10.2f %18d\n" mname (tmed *. 1000.) live)
    modes
;;

(* -- scale sweep ----------------------------------------------------------- *)

(* The per-entry columns are the result: they should hold flat as the table
   grows, and their value at any one size says little.

   [ns/intern] climbing means the bucket walk is lengthening, so the hash is not
   spreading or the resize is falling behind the load factor. [B/intern]
   climbing means per-entry overhead grows with occupancy.

   Unique literals throughout, so nothing dedups away and [entries] tracks the
   work done. Hashconsed only; Plain has no table and so no scaling question.

   One timed run per size rather than the median used elsewhere: at the top size
   the repeats cost more than the precision buys. *)
let bench_scale () =
  let sizes = [ 10_000; 40_000; 160_000; 640_000 ] in
  Printf.printf "\n## scale sweep (unique literals, Hashconsed)\n";
  Printf.printf
    "%9s %10s %10s %9s %10s %9s %9s %8s %8s\n"
    "roots"
    "nodes"
    "tokens"
    "time(ms)"
    "ns/intern"
    "B/intern"
    "buckets"
    "median"
    "biggest";
  List.iter
    (fun n ->
       let shapes = corpus ~n ~max_depth:8 ~lit:(make_unique_lit ()) in
       (* [shapes] is allocated and reachable before the baseline, so it drops
          out of the delta and only the green tree is measured. *)
       Gc.full_major ();
       let l0 = Gc.((stat ()).live_words) in
       let t0 = Sys.time () in
       let cache, roots = build_corpus hashconsed shapes in
       let dt = Sys.time () -. t0 in
       Gc.full_major ();
       let l1 = Gc.((stat ()).live_words) in
       let ns = Cache.node_stats cache
       and ts = Cache.token_stats cache in
       let interns = ns.Cache.entries + ts.Cache.entries in
       Printf.printf
         "%9d %10d %10d %9.1f %10.1f %9.1f %9d %8d %8d\n"
         n
         ns.Cache.entries
         ts.Cache.entries
         (dt *. 1000.)
         (dt *. 1e9 /. float_of_int interns)
         (float_of_int ((l1 - l0) * word_bytes) /. float_of_int interns)
         ns.Cache.table_length
         ns.Cache.median_bucket
         ns.Cache.biggest_bucket;
       (* Hold both past the stats read, or the GC reaps the tree mid-row and
          the whole table reads as empty. *)
       ignore (Sys.opaque_identity roots);
       ignore (Sys.opaque_identity shapes))
    sizes
;;

(* -- re-parse through a warm cache ----------------------------------------- *)

(* What a keystroke costs when the editor re-parses the whole file and the
   previous tree is still alive.

   [cold] is the first parse, every intern a miss. [warm] replays the identical
   input, so every intern hits. [edit] changes one literal in the whole corpus,
   missing on that token and the spine above it and hitting everywhere else.

   Source bytes are the column to read: node counts do not map to file sizes by
   eye. This times tree construction only, so it is a floor on a real re-parse,
   which also lexes.

   The previous roots stay alive across the later runs. Drop them and the weak
   table reaps the entries, so [warm] measures another cold build. *)
let bench_reparse () =
  let edit_first_leaf shape =
    let done_ = ref false in
    let rec go s =
      match s with
      | Leaf (kind, t) when not !done_ ->
        done_ := true;
        Leaf (kind, t ^ "z")
      | Leaf _ -> s
      | Branch (kind, cs) -> Branch (kind, List.map go cs)
    in
    go shape
  in
  Printf.printf "\n## re-parse through a warm cache (32-literal vocab)\n";
  Printf.printf
    "%9s %11s %10s %10s %10s %10s\n"
    "roots"
    "source(KB)"
    "nodes"
    "cold(ms)"
    "warm(ms)"
    "edit(ms)";
  List.iter
    (fun n ->
       let shapes = corpus ~n ~max_depth:8 ~lit:shared_lit in
       let cache = Cache.create () in
       let build cs = snd (build_corpus (fun () -> cache) cs) in
       let t0 = Sys.time () in
       let first = build shapes in
       let cold = Sys.time () -. t0 in
       let t1 = Sys.time () in
       let again = build shapes in
       let warm = Sys.time () -. t1 in
       let edited = Array.copy shapes in
       edited.(0) <- edit_first_leaf shapes.(0);
       let t2 = Sys.time () in
       let after = build edited in
       let edit = Sys.time () -. t2 in
       let bytes = Array.fold_left (fun acc r -> acc + Green.text_len r) 0 first in
       Printf.printf
         "%9d %11.1f %10d %10.2f %10.2f %10.2f\n"
         n
         (float_of_int bytes /. 1e3)
         Cache.((node_stats cache).entries)
         (cold *. 1000.)
         (warm *. 1000.)
         (edit *. 1000.);
       ignore (Sys.opaque_identity (first, again, after)))
    [ 2_000; 10_000; 50_000 ]
;;

(* -- entry point ----------------------------------------------------------- *)

let () =
  let scale = Array.exists (String.equal "--scale") Sys.argv in
  Printf.printf "Siesta Dedup/Cache baseline, %d-bit words\n" (word_bytes * 8);
  Printf.printf "alloc + foot are exact; time is median of 3 reps (CPU seconds, noisy).\n";
  let n = 20_000
  and max_depth = 8 in
  bench_corpus "high sharing (32-literal vocab)" (corpus ~n ~max_depth ~lit:shared_lit);
  bench_corpus
    "low sharing (unique literals)"
    (corpus ~n ~max_depth ~lit:(make_unique_lit ()));
  bench_wide ();
  bench_deep ();
  bench_churn ();
  bench_reparse ();
  if scale then bench_scale ()
;;
