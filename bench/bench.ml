(* Micro-benchmark harness for Dedup / Cache.

   Allocation and footprint are exact and deterministic. [alloc] is the
   [Gc.allocated_bytes] delta around a workload, garbage included, so it reads
   as GC pressure. [foot] is the [Gc.stat().live_words] delta while the result
   is held alive, so it reads as retained data.

   Time is the noisy one, reported as the median of a few reps after a warm-up,
   inside the same run so the comparison holds.

   Corpora come from a fixed seed, so runs are comparable and both cache modes
   see byte-identical inputs.

   Run: dune exec bench/bench.exe *)

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

(* -- entry point ----------------------------------------------------------- *)

let () =
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
  bench_churn ()
;;
