# Parser2 Optimization Plan

Optimizing `bench/src/Bytes/Parser2.elm` (a copy of `zwilias/elm-bytes-parser`) for maximum performance while preserving the same API and error reporting semantics.

## Architecture

Parser2 uses a pure state-passing function approach:

```elm
type Parser context error value
    = Parser (State -> ParseResult context error value)

type ParseResult context error value
    = Good value State
    | Bad (Error context error)

type alias State =
    { offset : Int, stack : List Int, input : Bytes }
```

Every primitive re-enters `Decode.decode` from scratch, skipping `offset` bytes each time via `Decode.map2 (always identity) (Decode.bytes state.offset) dec`. This is inherent to the approach and cannot be changed without a fundamental redesign.

## Optimization Opportunities

### 1. Direct `map2` implementation — HIGH impact

- [x] Implement

**Current** (line 509-510):
```elm
map2 f parserX parserY =
    parserX |> andThen (\x -> parserY |> andThen (\y -> succeed (f x y)))
```

This chains through 2× `andThen` + 1× `succeed`, meaning: unwrap `Parser`, case-match `ParseResult`, wrap a new `Parser` closure, unwrap again, case-match again, allocate a `Good` via `succeed`. Each step allocates intermediate closures and `Parser` wrappers.

**Optimized**: Direct nested case with no intermediate wrapping:
```elm
map2 f (Parser fx) (Parser fy) =
    Parser <| \state ->
        case fx state of
            Good x s1 ->
                case fy s1 of
                    Good y s2 -> Good (f x y) s2
                    Bad e -> Bad e
            Bad e -> Bad e
```

**Why it matters**: `map2` is the foundation of all applicative composition — `map3`–`map5`, `keep`, `ignore` all call through it. Every pipeline step hits this.

### 2. Direct `map3`–`map5` implementations — HIGH impact

- [x] Implement

**Current**: `map5` chains `map4 → map3 → map2 → keep → map2`, each adding a layer of `Parser` wrapping/unwrapping and closure allocation.

**Optimized**: Flat nested `case` matching, e.g. `map5` has 5 nested cases with a single `Good` at the end — no intermediate `Parser` values at all.

### 3. Direct `keep`/`ignore` — MEDIUM impact

- [x] Implement

**Current**:
```elm
keep val fun = map2 (<|) fun val
ignore skipper keeper = map2 always keeper skipper
```

With optimized `map2` these improve automatically, but they still go through the `(<|)` / `always` function indirection. Inlining removes that layer.

### 4. Remove `stack` from State — LOW-MEDIUM impact

- [x] Implement

`stack : List Int` is never read or written anywhere in the codebase. Every `{ state | offset = ... }` record update copies it. Removing it shrinks every state allocation from 3 fields to 2.

### 5. `fromDecoder` fast path when offset == 0 — LOW-MEDIUM impact

- [ ] Implement

**Current** (line 888-890): Always constructs `Decode.map2 (always identity) (Decode.bytes state.offset) dec` even when `offset == 0`.

**Optimized**: Check `state.offset == 0` and use `dec` directly, skipping the `map2` + `bytes 0` allocation. Benefits the first primitive in every decoder chain.

### 6. Direct `repeat` without going through `loop` — LOW impact

- [ ] Implement

**Current**: `repeat` delegates to `loop` + `repeatHelp`, which unwraps `Parser` every iteration and pattern-matches `Step`.

**Optimized**: A direct tail-recursive helper that avoids `Parser` wrapping on each iteration.

## Benchmarking

Compare `zw_*` (original) vs `p2_*` (optimized) using `BenchZw.elm`:

```sh
cd bench
elm-bench -f BenchZw.zw_map5 -f BenchZw.p2_map5 "()"
elm-bench -f BenchZw.zw_loop_100 -f BenchZw.p2_loop_100 "()"
elm-bench -f BenchZw.zw_andThen_5 -f BenchZw.p2_andThen_5 "()"
elm-bench -f BenchZw.zw_oneOf_first -f BenchZw.p2_oneOf_first "()"
elm-bench -f BenchZw.zw_oneOf_last -f BenchZw.p2_oneOf_last "()"
elm-bench -f BenchZw.zw_packet -f BenchZw.p2_packet "()"
elm-bench -f BenchZw.zw_message -f BenchZw.p2_message "()"
elm-bench -f BenchZw.zw_tagged50 -f BenchZw.p2_tagged50 "()"
```
