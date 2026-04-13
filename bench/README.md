# bench/

Benchmarks comparing Elm bytes decoding approaches.


## Approaches

| Prefix | Module / Package | Strategy | Returns |
|--------|-----------------|----------|---------|
| `raw_` | elm/bytes (baseline) | Single `Decoder` | `Maybe` |
| `bd_`  | elm-cardano/bytes-decoder | Dual fast `Decoder` + slow `State -> ParseResult` | `Result` → `Maybe` |
| `zw_`  | zwilias/elm-bytes-parser | `State -> ParseResult` | `Result` → `Maybe` |
| `br_`  | mpizenberg/elm-bytes-decoder | Branchable `Decoder` | `Maybe` |

> **Note:** Earlier iterations of this benchmark suite included `Parser2` (an
> optimized pure state-passing parser) and `Parser3` (a dual-path prototype) as
> local modules. Those were used historically during optimization experiments to
> guide the design of the current `Bytes.Decoder` implementation and have since
> been removed.


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

# Realistic 1 — all-fast packet (48 B, pure applicative)
elm-bench -f Bench.raw_packet -f Bench.bd_packet -f Bench.zw_packet -f Bench.br_packet "()"

# Realistic 2 — dynamic message (57 B, andThen + loop)
elm-bench -f Bench.raw_message -f Bench.bd_message -f Bench.zw_message -f Bench.br_message "()"

# Realistic 3 — 50 tagged records with oneOf (175 B)
elm-bench -f Bench.raw_tagged50 -f Bench.bd_tagged50 -f Bench.zw_tagged50 -f Bench.br_tagged50 "()"
```
