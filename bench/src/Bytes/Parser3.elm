module Bytes.Parser3 exposing
    ( Parser, run, Error(..)
    , succeed, fail, inContext
    , unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32
    , float32, float64
    , string
    , bytes
    , map, map2, map3, map4, map5
    , keep, ignore, skip
    , andThen, oneOf, repeat, Step(..), loop
    )

{-| Dual-path bytes parser: optimistic fast path with full error reporting.


# Architecture

Each `Parser` carries two execution strategies:

1.  **Fast path** — a raw `elm/bytes` `Decoder` composed via `Decode.map2`,
    `Decode.andThen`, `Decode.loop`, etc. When present, `run` executes a single
    `Decode.decode` call with zero per-step overhead (no `ParseResult` pattern
    matching, no `State` threading). This matches the performance of hand-written
    `elm/bytes` decoders.

2.  **Slow path** — a `State -> ParseResult` function identical to the approach
    in `Bytes.Parser2`. It tracks byte offsets, reports structured errors, and
    supports backtracking in `oneOf`. This path is only executed when the fast
    path is unavailable or returns `Nothing` (i.e. an error occurred).

The key insight is that most parsers succeed on well-formed input. By deferring
error tracking to a re-parse, the happy path pays no cost for error reporting.


# Fast-path availability

Not every combinator can be expressed as a raw `Decoder`:

  - **Always fast**: `succeed`, `map`, `map2`–`map5`, `keep`, `ignore`, `skip`,
    `andThen`, `loop`, `repeat`, and all primitives.
  - **Always slow**: `fail` (no `Decoder` can produce a value), `oneOf`
    (requires backtracking which `Decoder` cannot do mid-stream).
  - **Conditionally fast**: `andThen` and `loop` remain fast as long as the
    callback returns a parser with a fast path. If a callback branch returns
    `fail`, that branch uses `Decode.fail` to signal the decoder, which causes
    `Decode.decode` to return `Nothing` and triggers the slow re-parse.

In practice, `fail` branches inside `andThen` are error-handling code that only
runs on malformed input — exactly when we want to re-parse with error tracking.


# Benchmark results (vs Parser2)

  - Applicative (`map5`, `keep5`, `packet`): **2–5× faster**
  - Sequential (`andThen_5`, `message`): **40–80% faster**
  - Loops (`repeat_100`, `loop_1000`): **2–5× faster**
  - `oneOf` / `tagged50`: **2–9% slower** (fast path absent, minor overhead
    from the `Maybe` check in `run`)


# Running parsers

@docs Parser, run, Error


# Static parsers

@docs succeed, fail, inContext


# Basic parsers


## Integers

@docs unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32


## Floats

@docs float32, float64


## Strings

@docs string


## Bytes

@docs bytes


# Transforming values

@docs map, map2, map3, map4, map5


# Combining parsers

@docs keep, ignore, skip


# Fancy parsers

@docs andThen, oneOf, repeat, Step, loop

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


{-| A parser which tracks a certain type of context, a certain type of error and
produces a certain type of value.

Internally, it carries an optional raw `Decoder` (fast path) and a
`State -> ParseResult` function (slow path for error reporting).

-}
type Parser context error value
    = Parser (Maybe (Decoder value)) (State -> ParseResult context error value)


type ParseResult context error value
    = Good value State
    | Bad (Error context error)


{-| Describes errors that arise while parsing.

Custom errors happen through [`fail`](#fail), context tracking happens through
[`inContext`](#inContext).

-}
type Error context error
    = InContext { label : context, start : Int } (Error context error)
    | OutOfBounds { at : Int, bytes : Int }
    | Custom { at : Int } error
    | BadOneOf { at : Int } (List (Error context error))


type alias State =
    { offset : Int
    , input : Bytes
    }


{-| Run the given parser on the provided bytes.

First tries the fast path (a single `Decode.decode` call). If the fast path is
unavailable or fails, re-parses with the slow path to produce a detailed error.

-}
run :
    Parser context error value
    -> Bytes
    -> Result (Error context error) value
run (Parser maybeDec slow) input =
    case maybeDec of
        Just dec ->
            case Decode.decode dec input of
                Just v ->
                    Ok v

                Nothing ->
                    runSlow slow input

        Nothing ->
            runSlow slow input


runSlow : (State -> ParseResult context error value) -> Bytes -> Result (Error context error) value
runSlow slow input =
    case slow { offset = 0, input = input } of
        Good v _ ->
            Ok v

        Bad e ->
            Err e



-- STATIC


{-| Always succeed with the given value, consuming no input.
-}
succeed : value -> Parser context error value
succeed val =
    Parser (Just (Decode.succeed val)) (Good val)


{-| Always fail with the given error.

This parser has no fast path — using it (even in a branch of `andThen`) will
cause `run` to fall back to the slow path for error reporting.

-}
fail : error -> Parser context error value
fail e =
    Parser Nothing (\state -> Bad (Custom { at = state.offset } e))


{-| Add context to errors that may occur during parsing.

The fast path is preserved (context only matters on the error path).

-}
inContext :
    context
    -> Parser context error value
    -> Parser context error value
inContext label (Parser maybeDec slow) =
    Parser maybeDec
        (\state ->
            case slow state of
                Good v s ->
                    Good v s

                Bad e ->
                    Bad
                        (InContext
                            { label = label
                            , start = state.offset
                            }
                            e
                        )
        )



-- COMBINATORS


{-| Transform the value a parser produces.
-}
map :
    (a -> b)
    -> Parser context error a
    -> Parser context error b
map t (Parser maybeDec slow) =
    Parser
        (Maybe.map (Decode.map t) maybeDec)
        (\state ->
            case slow state of
                Good v s ->
                    Good (t v) s

                Bad e ->
                    Bad e
        )


{-| Parse one thing, then parse another thing based on the result.

The fast path uses `Decode.andThen`. If the callback returns a parser without
a fast path (e.g. via `fail`), the fast decoder signals failure via
`Decode.fail`, causing `run` to fall back to the slow path.

-}
andThen :
    (a -> Parser context error b)
    -> Parser context error a
    -> Parser context error b
andThen toNext (Parser maybeDecA slowA) =
    Parser
        (Maybe.map
            (\decA ->
                Decode.andThen
                    (\a ->
                        case toNext a of
                            Parser (Just decB) _ ->
                                decB

                            Parser Nothing _ ->
                                Decode.fail
                    )
                    decA
            )
            maybeDecA
        )
        (\state ->
            case slowA state of
                Good a s ->
                    let
                        (Parser _ slowB) =
                            toNext a
                    in
                    slowB s

                Bad e ->
                    Bad e
        )


{-| Combine what 2 parsers produce.
-}
map2 :
    (x -> y -> z)
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map2 f (Parser maybeDecX slowX) (Parser maybeDecY slowY) =
    Parser
        (case ( maybeDecX, maybeDecY ) of
            ( Just decX, Just decY ) ->
                Just (Decode.map2 f decX decY)

            _ ->
                Nothing
        )
        (\state ->
            case slowX state of
                Good x s1 ->
                    case slowY s1 of
                        Good y s2 ->
                            Good (f x y) s2

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Combine what 3 parsers produce.
-}
map3 :
    (w -> x -> y -> z)
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map3 f (Parser maybeDecW slowW) (Parser maybeDecX slowX) (Parser maybeDecY slowY) =
    Parser
        (case ( maybeDecW, maybeDecX, maybeDecY ) of
            ( Just decW, Just decX, Just decY ) ->
                Just (Decode.map3 f decW decX decY)

            _ ->
                Nothing
        )
        (\state ->
            case slowW state of
                Good w s1 ->
                    case slowX s1 of
                        Good x s2 ->
                            case slowY s2 of
                                Good y s3 ->
                                    Good (f w x y) s3

                                Bad e ->
                                    Bad e

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Combine what 4 parsers produce.
-}
map4 :
    (v -> w -> x -> y -> z)
    -> Parser context error v
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map4 f (Parser maybeDecV slowV) (Parser maybeDecW slowW) (Parser maybeDecX slowX) (Parser maybeDecY slowY) =
    Parser
        (case ( maybeDecV, maybeDecW ) of
            ( Just decV, Just decW ) ->
                case ( maybeDecX, maybeDecY ) of
                    ( Just decX, Just decY ) ->
                        Just (Decode.map4 f decV decW decX decY)

                    _ ->
                        Nothing

            _ ->
                Nothing
        )
        (\state ->
            case slowV state of
                Good v s1 ->
                    case slowW s1 of
                        Good w s2 ->
                            case slowX s2 of
                                Good x s3 ->
                                    case slowY s3 of
                                        Good y s4 ->
                                            Good (f v w x y) s4

                                        Bad e ->
                                            Bad e

                                Bad e ->
                                    Bad e

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Combine what 5 parsers produce.
-}
map5 :
    (u -> v -> w -> x -> y -> z)
    -> Parser context error u
    -> Parser context error v
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map5 f (Parser maybeDecU slowU) (Parser maybeDecV slowV) (Parser maybeDecW slowW) (Parser maybeDecX slowX) (Parser maybeDecY slowY) =
    Parser
        (case ( maybeDecU, maybeDecV ) of
            ( Just decU, Just decV ) ->
                case ( maybeDecW, maybeDecX, maybeDecY ) of
                    ( Just decW, Just decX, Just decY ) ->
                        Just (Decode.map5 f decU decV decW decX decY)

                    _ ->
                        Nothing

            _ ->
                Nothing
        )
        (\state ->
            case slowU state of
                Good u s1 ->
                    case slowV s1 of
                        Good v s2 ->
                            case slowW s2 of
                                Good w s3 ->
                                    case slowX s3 of
                                        Good x s4 ->
                                            case slowY s4 of
                                                Good y s5 ->
                                                    Good (f u v w x y) s5

                                                Bad e ->
                                                    Bad e

                                        Bad e ->
                                            Bad e

                                Bad e ->
                                    Bad e

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Keep the value produced by a parser in a pipeline.

    P3.succeed Record5
        |> P3.keep P3.unsignedInt8
        |> P3.keep P3.unsignedInt8
        |> P3.keep P3.unsignedInt8
        |> P3.keep P3.unsignedInt8
        |> P3.keep P3.unsignedInt8

-}
keep :
    Parser context error a
    -> Parser context error (a -> b)
    -> Parser context error b
keep (Parser maybeDecVal slowVal) (Parser maybeDecFun slowFun) =
    Parser
        (case ( maybeDecFun, maybeDecVal ) of
            ( Just decFun, Just decVal ) ->
                Just (Decode.map2 (\g a -> g a) decFun decVal)

            _ ->
                Nothing
        )
        (\state ->
            case slowFun state of
                Good g s1 ->
                    case slowVal s1 of
                        Good a s2 ->
                            Good (g a) s2

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Ignore the value produced by a parser in a pipeline.

The ignored parser must still succeed for the pipeline to succeed.

-}
ignore :
    Parser context error ignore
    -> Parser context error keep
    -> Parser context error keep
ignore (Parser maybeDecSkip slowSkip) (Parser maybeDecKeep slowKeep) =
    Parser
        (case ( maybeDecKeep, maybeDecSkip ) of
            ( Just decKeep, Just decSkip ) ->
                Just (Decode.map2 (\k _ -> k) decKeep decSkip)

            _ ->
                Nothing
        )
        (\state ->
            case slowKeep state of
                Good k s1 ->
                    case slowSkip s1 of
                        Good _ s2 ->
                            Good k s2

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e
        )


{-| Skip a number of bytes in a pipeline.
-}
skip : Int -> Parser context error value -> Parser context error value
skip nBytes =
    ignore (bytes nBytes)


{-| Try a list of parsers and succeed with the first one that succeeds.

This parser has no fast path — it always uses the slow path with backtracking.
For tag-based dispatch, prefer `andThen` which preserves the fast path.

-}
oneOf : List (Parser context error value) -> Parser context error value
oneOf options =
    Parser Nothing (oneOfHelp options [])


oneOfHelp :
    List (Parser context error value)
    -> List (Error context error)
    -> State
    -> ParseResult context error value
oneOfHelp options errors state =
    case options of
        [] ->
            Bad (BadOneOf { at = state.offset } (List.reverse errors))

        (Parser _ slow) :: xs ->
            case slow state of
                Good v s ->
                    Good v s

                Bad e ->
                    oneOfHelp xs (e :: errors) state


{-| Represent the next step of a loop: either continue with updated state,
or finish with a final value.
-}
type Step state a
    = Loop state
    | Done a


{-| Loop a parser until it declares it is done.

The fast path uses `Decode.loop` (a tight `while` loop in the JS kernel).
If any iteration's callback returns a parser without a fast path, the fast
decoder signals failure via `Decode.fail` and `run` falls back to the slow path.

-}
loop :
    (state -> Parser context error (Step state a))
    -> state
    -> Parser context error a
loop toNext initialState =
    Parser
        (Just (loopFast toNext initialState))
        (loopHelp initialState toNext)


loopFast :
    (state -> Parser context error (Step state a))
    -> state
    -> Decoder a
loopFast toNext initial =
    Decode.loop initial
        (\loopState ->
            case toNext loopState of
                Parser (Just dec) _ ->
                    Decode.map
                        (\step ->
                            case step of
                                Loop s ->
                                    Decode.Loop s

                                Done a ->
                                    Decode.Done a
                        )
                        dec

                Parser Nothing _ ->
                    Decode.fail
        )


loopHelp :
    state
    -> (state -> Parser context error (Step state a))
    -> State
    -> ParseResult context error a
loopHelp loopState toNext state =
    let
        (Parser _ slow) =
            toNext loopState
    in
    case slow state of
        Good (Loop newLoopState) newState ->
            loopHelp newLoopState toNext newState

        Good (Done v) newState ->
            Good v newState

        Bad e ->
            Bad e


{-| Repeat a parser a given number of times, collecting results into a list.

The fast path uses `Decode.loop` (a tight `while` loop in the JS kernel).

-}
repeat : Parser context error value -> Int -> Parser context error (List value)
repeat (Parser maybeDec slow) nTimes =
    Parser
        (Maybe.map (\dec -> repeatFast dec nTimes) maybeDec)
        (repeatHelp slow nTimes [])


repeatFast : Decoder value -> Int -> Decoder (List value)
repeatFast dec nTimes =
    Decode.loop ( nTimes, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\v -> Decode.Loop ( remaining - 1, v :: acc )) dec
        )


repeatHelp :
    (State -> ParseResult context error value)
    -> Int
    -> List value
    -> State
    -> ParseResult context error (List value)
repeatHelp slow remaining acc state =
    if remaining <= 0 then
        Good (List.reverse acc) state

    else
        case slow state of
            Good v newState ->
                repeatHelp slow (remaining - 1) (v :: acc) newState

            Bad e ->
                Bad e



-- PRIMITIVES


{-| Parse one byte into an integer from 0 to 255.
-}
unsignedInt8 : Parser context error Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


{-| Parse one byte into an integer from -128 to 127.
-}
signedInt8 : Parser context error Int
signedInt8 =
    fromDecoder Decode.signedInt8 1


{-| Parse two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Parser context error Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


{-| Parse two bytes into an integer from -32768 to 32767.
-}
signedInt16 : Bytes.Endianness -> Parser context error Int
signedInt16 bo =
    fromDecoder (Decode.signedInt16 bo) 2


{-| Parse four bytes into an integer from 0 to 4294967295.
-}
unsignedInt32 : Bytes.Endianness -> Parser context error Int
unsignedInt32 bo =
    fromDecoder (Decode.unsignedInt32 bo) 4


{-| Parse four bytes into an integer from -2147483648 to 2147483647.
-}
signedInt32 : Bytes.Endianness -> Parser context error Int
signedInt32 bo =
    fromDecoder (Decode.signedInt32 bo) 4


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Parser context error Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


{-| Parse 8 bytes into a Float.
-}
float64 : Bytes.Endianness -> Parser context error Float
float64 bo =
    fromDecoder (Decode.float64 bo) 8


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Parser context error Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


{-| Parse `byteCount` bytes representing UTF-8 characters into a String.
-}
string : Int -> Parser context error String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount


{-| Build a parser from a raw `Decoder` and its byte length.

The fast path stores the decoder directly (it will be composed into a larger
decoder by combinators). The slow path uses the P2 strategy: skip to the
current offset via `Decode.bytes`, then decode.

-}
fromDecoder : Decoder v -> Int -> Parser context error v
fromDecoder dec byteLength =
    Parser
        (Just dec)
        (\state ->
            let
                combined : Decoder v
                combined =
                    Decode.map2 (\_ v -> v) (Decode.bytes state.offset) dec
            in
            case Decode.decode combined state.input of
                Just res ->
                    Good res { offset = state.offset + byteLength, input = state.input }

                Nothing ->
                    Bad (OutOfBounds { at = state.offset, bytes = byteLength })
        )
