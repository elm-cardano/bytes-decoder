# bench/

Benchmarks comparing four Elm bytes decoding approaches:

| Prefix | Package | Returns |
|--------|---------|---------|
| `raw_` | elm/bytes (baseline) | `Maybe` |
| `bd_`  | elm-cardano/bytes-decoder (this package) | `Result` → `Maybe` |
| `zw_`  | zwilias/elm-bytes-parser | `Result` → `Maybe` |
| `br_`  | mpizenberg/elm-bytes-decoder (Branchable) | `Maybe` |

## Running benchmarks

Using [elm-bench](https://github.com/elm-menagerie/elm-bench):

```sh
cd bench

# Sequential (applicative) — map5, 5 fields
elm-bench -f Bench.raw_map5 -f Bench.bd_map5 -f Bench.zw_map5 -f Bench.br_map5 "()"

# Loop — 100 float64s
elm-bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 -f Bench.zw_loop_100 -f Bench.br_loop_100 "()"

# Loop — 1000 float64s
elm-bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 -f Bench.zw_loop_1000 -f Bench.br_loop_1000 "()"

# andThen — 5 fields via chained andThen
elm-bench -f Bench.raw_andThen_5 -f Bench.bd_andThen_5 -f Bench.zw_andThen_5 -f Bench.br_andThen_5 "()"

# oneOf — first alternative
elm-bench -f Bench.bd_oneOf_first -f Bench.zw_oneOf_first -f Bench.br_oneOf_first "()"

# oneOf — last alternative (worst case)
elm-bench -f Bench.bd_oneOf_last -f Bench.zw_oneOf_last -f Bench.br_oneOf_last "()"
```
