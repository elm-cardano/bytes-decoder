# bench/

Benchmarks comparing Elm bytes decoding approaches.


## Approaches

| Prefix | Module / Package | Strategy | Returns |
|--------|-----------------|----------|---------|
| `raw_` | elm/bytes (baseline) | Single `Decoder` | `Maybe` |
| `bd_`  | elm-cardano/bytes-decoder | Single `Decoder` | `Result` → `Maybe` |
| `zw_`  | zwilias/elm-bytes-parser | `State -> ParseResult` | `Result` → `Maybe` |
| `p2_`  | Bytes.Parser2 (optimized zw) | `State -> ParseResult` | `Result` → `Maybe` |
| `p3_`  | Bytes.Parser3 (dual-path) | Fast `Decoder` + slow `State -> ParseResult` | `Result` → `Maybe` |
| `br_`  | mpizenberg/elm-bytes-decoder | Branchable `Decoder` | `Maybe` |


## Parser3: dual-path architecture

Parser3 carries two execution strategies inside each `Parser`:

1. **Fast path** — a raw `elm/bytes` `Decoder` composed via `Decode.map2`,
   `Decode.andThen`, `Decode.loop`, etc. When present, `run` executes a single
   `Decode.decode` call with no per-step overhead. This matches the performance
   of hand-written `elm/bytes` decoders.

2. **Slow path** — a `State -> ParseResult` function (same as Parser2). It
   tracks byte offsets, reports structured errors, and supports backtracking.
   Only executed when the fast path is unavailable or fails.

### Fast-path availability by combinator

| Combinator | Fast path | Mechanism |
|------------|-----------|-----------|
| `succeed`, `map`, `map2`–`map5` | Always | `Decode.succeed`, `Decode.map`, `Decode.map2`–`map5` |
| `keep`, `ignore`, `skip` | Always | `Decode.map2` |
| `andThen` | Conditional | `Decode.andThen`; uses `Decode.fail` if callback returns no fast path |
| `loop`, `repeat` | Always | `Decode.loop` (kernel `while` loop) |
| `fail` | Never | No `Decoder` can produce a value of arbitrary type |
| `oneOf` | Never | `Decoder` cannot backtrack mid-stream |

### Benchmark results (P3 vs P2)

| Benchmark | Speedup |
|-----------|---------|
| Applicative (`map5`, `keep5`, `packet`) | 2–5× faster |
| Sequential (`andThen_5`, `message`) | 40–80% faster |
| Loops (`repeat_100`, `loop_1000`) | 2–5× faster |
| `oneOf` / `tagged50` | 2–9% slower |


## Running benchmarks

Using [elm-bench](https://github.com/elm-menagerie/elm-bench):

```sh
cd bench

# Sequential (applicative) — map5, 5 fields
elm-bench -f Bench.raw_map5 -f Bench.bd_map5 -f Bench.zw_map5 -f Bench.p2_map5 -f Bench.br_map5 "()"

# Loop — 100 float64s
elm-bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 -f Bench.zw_loop_100 -f Bench.p2_loop_100 -f Bench.br_loop_100 "()"

# Loop — 1000 float64s
elm-bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 -f Bench.zw_loop_1000 -f Bench.p2_loop_1000 -f Bench.br_loop_1000 "()"

# andThen — 5 fields via chained andThen
elm-bench -f Bench.raw_andThen_5 -f Bench.bd_andThen_5 -f Bench.zw_andThen_5 -f Bench.p2_andThen_5 -f Bench.br_andThen_5 "()"

# oneOf — first alternative
elm-bench -f Bench.bd_oneOf_first -f Bench.zw_oneOf_first -f Bench.p2_oneOf_first -f Bench.br_oneOf_first "()"

# oneOf — last alternative (worst case)
elm-bench -f Bench.bd_oneOf_last -f Bench.zw_oneOf_last -f Bench.p2_oneOf_last -f Bench.br_oneOf_last "()"

# Realistic 1 — all-fast packet (48 B, pure applicative)
elm-bench -f Bench.raw_packet -f Bench.bd_packet -f Bench.zw_packet -f Bench.p2_packet -f Bench.br_packet "()"

# Realistic 2 — dynamic message (57 B, andThen + loop)
elm-bench -f Bench.raw_message -f Bench.bd_message -f Bench.zw_message -f Bench.p2_message -f Bench.br_message "()"

# Realistic 3 — 50 tagged records with oneOf (175 B)
elm-bench -f Bench.raw_tagged50 -f Bench.bd_tagged50 -f Bench.zw_tagged50 -f Bench.p2_tagged50 -f Bench.br_tagged50 "()"
```

### P2 vs P3 head-to-head

```sh
cd bench

elm-bench -f BenchZw.p2_map2 -f BenchZw.p3_map2 "()"
elm-bench -f BenchZw.p2_keep5 -f BenchZw.p3_keep5 "()"
elm-bench -f BenchZw.p2_map5 -f BenchZw.p3_map5 "()"
elm-bench -f BenchZw.p2_repeat_100 -f BenchZw.p3_repeat_100 "()"
elm-bench -f BenchZw.p2_loop_10 -f BenchZw.p3_loop_10 "()"
elm-bench -f BenchZw.p2_loop_100 -f BenchZw.p3_loop_100 "()"
elm-bench -f BenchZw.p2_loop_1000 -f BenchZw.p3_loop_1000 "()"
elm-bench -f BenchZw.p2_andThen_5 -f BenchZw.p3_andThen_5 "()"
elm-bench -f BenchZw.p2_oneOf_first -f BenchZw.p3_oneOf_first "()"
elm-bench -f BenchZw.p2_oneOf_last -f BenchZw.p3_oneOf_last "()"
elm-bench -f BenchZw.p2_packet -f BenchZw.p3_packet "()"
elm-bench -f BenchZw.p2_message -f BenchZw.p3_message "()"
elm-bench -f BenchZw.p2_tagged50 -f BenchZw.p3_tagged50 "()"
```
