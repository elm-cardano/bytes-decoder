# bench/

Benchmarks comparing raw `elm/bytes` decoding vs `Bytes.Decoder`.

## Running benchmarks

Using [elm-bench](https://github.com/elm-menagerie/elm-bench):

```sh
cd bench

# Sequential (applicative) — map5, 5 fields
elm-bench -f Bench.raw_map5 -f Bench.bd_map5_fast -f Bench.bd_keep5_fast "()"

# Repeat / loop — 100 float64s
elm-bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 "()"

# Repeat / loop — 1000 float64s
elm-bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 "()"

# andThen (slow path) vs raw map5
elm-bench -f Bench.raw_map5 -f Bench.bd_andThen_5 "()"

# oneOf branching (first vs last alternative)
elm-bench -f Bench.bd_oneOf_first -f Bench.bd_oneOf_last "()"
```
