# bench/

Benchmarks comparing five Elm bytes decoding approaches:

| Prefix | Package | Returns |
|--------|---------|---------|
| `raw_` | elm/bytes (baseline) | `Maybe` |
| `bd_`  | elm-cardano/bytes-decoder (this package) | `Result` → `Maybe` |
| `zw_`  | zwilias/elm-bytes-parser (original) | `Result` → `Maybe` |
| `p2_`  | Bytes.Parser2 (optimized copy of zw) | `Result` → `Maybe` |
| `br_`  | mpizenberg/elm-bytes-decoder (Branchable) | `Maybe` |

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
