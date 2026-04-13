# elm-cardano/bytes-decoder

Fast bytes decoder for Elm with error reporting and `oneOf` branching.

```elm
import Bytes.Decoder as BD exposing (Error(..))

type alias Header =
    { version : Int, payloadLength : Int, tag : Int }

headerDecoder : BD.Decoder String String Header
headerDecoder =
    BD.inContext "header"
        (BD.succeed Header
            |> BD.keep BD.unsignedInt8
            |> BD.keep (BD.unsignedInt16 BE)
            |> BD.keep BD.unsignedInt8
        )
```

## Motivation

This package is an evolution of Ilias Van Peer's
[`zwilias/elm-bytes-parser`][zwilias], redesigned around a dual-path
architecture that preserves the error reporting and `oneOf` branching of the
original while recovering near-optimal performance on the happy path.

The built-in [`elm/bytes`][elm-bytes] decoder returns `Maybe`, giving no
indication of _what_ went wrong or _where_. This package provides:

- **Structured errors** with byte offsets (`OutOfBounds`, `Custom`,
  `BadOneOf`, `InContext`)
- **`oneOf` branching** with backtracking and collected error reports
- **Pipeline-style** API (`keep`, `ignore`, `skip`)
- **Near-zero overhead on the happy path** thanks to a dual-path architecture

[elm-bytes]: https://package.elm-lang.org/packages/elm/bytes/latest/
[zwilias]: https://package.elm-lang.org/packages/zwilias/elm-bytes-parser/latest/

## Performance

On realistic workloads, `bytes-decoder` (`bd`) stays within single-digit
percentage overhead of raw `elm/bytes` (`raw`), while `zwilias/elm-bytes-parser`
(`zw`) is 3-5x slower:

```
Realistic 1 — all-fast packet (48 B, pure applicative)

  Bench.raw_packet   ████████████████████   822 ns/run   baseline
  Bench.bd_packet    █████████████████████   869 ns/run
  Bench.zw_packet    ██████████████████████████████████ ... █████████████   3452 ns/run

Realistic 2 — dynamic message (57 B, andThen + loop)

  Bench.raw_message   ████████████████████   509 ns/run   baseline
  Bench.bd_message    ██████████████████████████████   756 ns/run
  Bench.zw_message    ██████████████████████████████████████████ ... ███████████   2949 ns/run
```

The `andThen`/`loop` scenario shows more overhead because every callback
re-enters the fast path via `Decode.decode`, but it is still significantly
faster than a pure state-passing approach. See the `bench/` directory for the
full suite.

## How it works

Each `Decoder` carries two execution strategies:

1. **Fast path** — a raw `elm/bytes` `Bytes.Decode.Decoder`. When available,
   `decode` runs a single `Decode.decode` call with zero per-step overhead,
   matching the performance of hand-written `elm/bytes` decoders.

2. **Slow path** — a state-passing function that tracks byte offsets, reports
   structured errors, and supports backtracking in `oneOf`. Only executed when
   the fast path is unavailable or fails.

Most combinators (`succeed`, `map`, `map2`–`map5`, `keep`, `ignore`, `skip`,
`andThen`, `loop`, `repeat`, and all primitives) always carry a fast path.
Only `fail` and `oneOf` force the slow path. Since `fail` branches inside
`andThen` only run on malformed input, the happy path pays no cost for error
reporting.

## Error reporting

Decode errors carry byte offsets and can be nested with context labels:

```elm
BD.decode headerDecoder truncatedInput
-- Err (InContext { label = "header", start = 0 }
--         (OutOfBounds { at = 1, bytes = 2 }))
```

The four error variants:

| Variant                            | Meaning                                                      |
| ---------------------------------- | ------------------------------------------------------------ |
| `OutOfBounds { at, bytes }`        | Tried to read `bytes` bytes at offset `at` past end of input |
| `Custom { at } error`              | User-defined error via `fail` at offset `at`                 |
| `BadOneOf { at } errors`           | All `oneOf` alternatives failed, with each sub-error         |
| `InContext { label, start } error` | Wraps an inner error with some context                       |

## Random access

Capture the current decode position and jump back to read data at arbitrary
offsets:

```elm
BD.succeed Tuple.pair
    |> BD.keep BD.unsignedInt8       -- sequential read at offset 0
    |> BD.keep
        (BD.randomAccess              -- jump to offset 3
            { offset = 3, relativeTo = BD.startOfInput }
            BD.unsignedInt8
        )
    |> BD.keep BD.unsignedInt8       -- resumes at offset 1
```

## Install

```sh
elm install elm-cardano/bytes-decoder
```

## Development

```sh
# Run tests
elm-test

# Run elm-review
elm-review

# Run benchmarks (requires elm-bench)
cd bench
elm-bench -f Bench.raw_map5 -f Bench.bd_map5 -f Bench.zw_map5 -f Bench.br_map5 "()"
```

See `bench/` for the full benchmark suite and `examples/` for error reporting
examples.

## License

BSD-3-Clause
