module Bytes.Decoder exposing
    ( Decoder, decode, Error(..)
    , succeed, fail, inContext
    , unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32
    , float16, float32, float64
    , string
    , bytes
    , map, map2, map3, map4, map5
    , keep, ignore, skip
    , andThen, oneOf, repeat, Step(..), loop
    , Position, position, startOfInput, randomAccess
    )

{-| Fast bytes decoder with error reporting and `oneOf` branching.


# Architecture

Each `Decoder` carries two execution strategies:

1.  **Fast path** — a raw `elm/bytes` `Bytes.Decode.Decoder` composed via
    `Decode.map2`, `Decode.andThen`, `Decode.loop`, etc. When present, `decode`
    executes a single `Decode.decode` call with zero per-step overhead. This
    matches the performance of hand-written `elm/bytes` decoders.

2.  **Slow path** — a state-passing function that tracks byte offsets, reports
    structured errors, and supports backtracking in `oneOf`. This path is only
    executed when the fast path is unavailable or returns `Nothing` (i.e. an
    error occurred).

The key insight is that most decoders succeed on well-formed input. By deferring
error tracking to a re-decode, the happy path pays no cost for error reporting.


# Fast-path availability

Not every combinator can be expressed as a raw `Bytes.Decode.Decoder`:

  - **Always fast**: `succeed`, `map`, `map2`–`map5`, `keep`, `ignore`, `skip`,
    `andThen`, `loop`, `repeat`, and all primitives.
  - **Always slow**: `fail` (no decoder can produce a value), `oneOf`
    (requires backtracking which `Bytes.Decode.Decoder` cannot do mid-stream).
  - **Conditionally fast**: `andThen` and `loop` remain fast as long as the
    callback returns a decoder with a fast path. If a callback branch returns
    `fail`, that branch uses `Decode.fail` to signal the raw decoder, which causes
    `Decode.decode` to return `Nothing` and triggers the slow re-decode.

In practice, `fail` branches inside `andThen` are error-handling code that only
runs on malformed input — exactly when we want to re-decode with error tracking.


# Running

@docs Decoder, decode, Error


# Static

@docs succeed, fail, inContext


# Primitives


## Integers

@docs unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32


## Floats

@docs float16, float32, float64


## Strings

@docs string


## Raw Bytes

@docs bytes


# Mapping

@docs map, map2, map3, map4, map5


# Pipeline

@docs keep, ignore, skip


# Chaining

@docs andThen, oneOf, repeat, Step, loop


# Random access

@docs Position, position, startOfInput, randomAccess

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Floating.Decode



-- TYPES


{-| A decoder that reads binary data and produces either a value or an error.

Internally, it carries an optional raw `Bytes.Decode.Decoder` (fast path) and a
state-passing function (slow path for error reporting).

-}
type Decoder context error value
    = Decoder (Maybe (Decode.Decoder value)) (State -> DecodeResult context error value)


type DecodeResult context error value
    = Good value State
    | Bad (Error context error)


{-| Describes errors that arise while decoding.

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



-- RUNNING


{-| Run the given decoder on the provided bytes.

First tries the fast path (a single `Decode.decode` call). If the fast path is
unavailable or fails, re-decodes with the slow path to produce a detailed error.

-}
decode :
    Decoder context error value
    -> Bytes
    -> Result (Error context error) value
decode (Decoder maybeDec slow) input =
    case maybeDec of
        Just dec ->
            case Decode.decode dec input of
                Just v ->
                    Ok v

                Nothing ->
                    decodeSlow slow input

        Nothing ->
            decodeSlow slow input


decodeSlow : (State -> DecodeResult context error value) -> Bytes -> Result (Error context error) value
decodeSlow slow input =
    case slow { offset = 0, input = input } of
        Good v _ ->
            Ok v

        Bad e ->
            Err e



-- STATIC


{-| Always succeed with the given value, consuming no input.
-}
succeed : value -> Decoder context error value
succeed val =
    Decoder (Just (Decode.succeed val)) (Good val)


{-| Always fail with the given error.

This decoder has no fast path — using it (even in a branch of `andThen`) will
cause `decode` to fall back to the slow path for error reporting.

-}
fail : error -> Decoder context error value
fail e =
    Decoder Nothing (\state -> Bad (Custom { at = state.offset } e))


{-| Add context to errors that may occur during decoding.

The fast path is preserved (context only matters on the error path).

-}
inContext :
    context
    -> Decoder context error value
    -> Decoder context error value
inContext label (Decoder maybeDec slow) =
    Decoder maybeDec
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



-- MAPPING


{-| Transform the value a decoder produces.
-}
map :
    (a -> b)
    -> Decoder context error a
    -> Decoder context error b
map t (Decoder maybeDec slow) =
    Decoder
        (Maybe.map (Decode.map t) maybeDec)
        (\state ->
            case slow state of
                Good v s ->
                    Good (t v) s

                Bad e ->
                    Bad e
        )


{-| Combine what 2 decoders produce.
-}
map2 :
    (x -> y -> z)
    -> Decoder context error x
    -> Decoder context error y
    -> Decoder context error z
map2 f (Decoder maybeDecX slowX) (Decoder maybeDecY slowY) =
    Decoder
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


{-| Combine what 3 decoders produce.
-}
map3 :
    (w -> x -> y -> z)
    -> Decoder context error w
    -> Decoder context error x
    -> Decoder context error y
    -> Decoder context error z
map3 f (Decoder maybeDecW slowW) (Decoder maybeDecX slowX) (Decoder maybeDecY slowY) =
    Decoder
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


{-| Combine what 4 decoders produce.
-}
map4 :
    (v -> w -> x -> y -> z)
    -> Decoder context error v
    -> Decoder context error w
    -> Decoder context error x
    -> Decoder context error y
    -> Decoder context error z
map4 f (Decoder maybeDecV slowV) (Decoder maybeDecW slowW) (Decoder maybeDecX slowX) (Decoder maybeDecY slowY) =
    Decoder
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


{-| Combine what 5 decoders produce.
-}
map5 :
    (u -> v -> w -> x -> y -> z)
    -> Decoder context error u
    -> Decoder context error v
    -> Decoder context error w
    -> Decoder context error x
    -> Decoder context error y
    -> Decoder context error z
map5 f (Decoder maybeDecU slowU) (Decoder maybeDecV slowV) (Decoder maybeDecW slowW) (Decoder maybeDecX slowX) (Decoder maybeDecY slowY) =
    Decoder
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



-- CHAINING


{-| Decode one thing, then decode another thing based on the result.

The fast path uses `Decode.andThen`. If the callback returns a decoder without
a fast path (e.g. via `fail`), the fast decoder signals failure via
`Decode.fail`, causing `decode` to fall back to the slow path.

-}
andThen :
    (a -> Decoder context error b)
    -> Decoder context error a
    -> Decoder context error b
andThen toNext (Decoder maybeDecA slowA) =
    Decoder
        (Maybe.map
            (\decA ->
                Decode.andThen
                    (\a ->
                        case toNext a of
                            Decoder (Just decB) _ ->
                                decB

                            Decoder Nothing _ ->
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
                        (Decoder _ slowB) =
                            toNext a
                    in
                    slowB s

                Bad e ->
                    Bad e
        )



-- PIPELINE


{-| Decode a value and apply it to the function held by the previous decoder.

    import Bytes.Decoder as BD

    BD.succeed Record
        |> BD.keep BD.unsignedInt8
        |> BD.keep BD.unsignedInt8
        |> BD.keep BD.unsignedInt8

-}
keep :
    Decoder context error a
    -> Decoder context error (a -> b)
    -> Decoder context error b
keep (Decoder maybeDecVal slowVal) (Decoder maybeDecFun slowFun) =
    Decoder
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


{-| Run a decoder but discard its result, keeping the previous value.

The ignored decoder must still succeed for the pipeline to succeed.

-}
ignore :
    Decoder context error ignore
    -> Decoder context error keep
    -> Decoder context error keep
ignore (Decoder maybeDecSkip slowSkip) (Decoder maybeDecKeep slowKeep) =
    Decoder
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


{-| Skip `n` bytes, then continue with the given decoder.
-}
skip : Int -> Decoder context error value -> Decoder context error value
skip nBytes =
    ignore (bytes nBytes)



-- BRANCHING


{-| Try each decoder in order, returning the first success.

This decoder has no fast path — it always uses the slow path with backtracking.
For tag-based dispatch, prefer `andThen` which preserves the fast path.

Collects all errors on failure for diagnostic purposes.

**Caution:** `oneOf` backtracks by byte offset, not by meaning. If two branches
consume the same number of bytes but interpret them differently (e.g.
`unsignedInt32` vs `float32`), the first branch that successfully reads those
bytes wins — even if the value makes no sense for your domain. In general,
raw byte sequences are untagged, so `oneOf` cannot distinguish between
alternatives that happen to have the same byte layout. Prefer `andThen` with
an explicit tag byte whenever possible.

-}
oneOf : List (Decoder context error value) -> Decoder context error value
oneOf options =
    Decoder Nothing (oneOfHelp options [])


oneOfHelp :
    List (Decoder context error value)
    -> List (Error context error)
    -> State
    -> DecodeResult context error value
oneOfHelp options errors state =
    case options of
        [] ->
            Bad (BadOneOf { at = state.offset } (List.reverse errors))

        (Decoder _ slow) :: xs ->
            case slow state of
                Good v s ->
                    Good v s

                Bad e ->
                    oneOfHelp xs (e :: errors) state



-- LOOPING


{-| Represent the next step of a loop: either continue with updated state,
or finish with a final value.
-}
type Step state a
    = Loop state
    | Done a


{-| Decode in a loop. Each iteration produces either `Loop` (continue with new
state) or `Done` (finish with a value).

The fast path uses `Decode.loop` (a tight `while` loop in the JS kernel).
If any iteration's callback returns a decoder without a fast path, the fast
decoder signals failure via `Decode.fail` and `decode` falls back to the slow
path.

-}
loop :
    (state -> Decoder context error (Step state a))
    -> state
    -> Decoder context error a
loop toNext initialState =
    Decoder
        (Just (loopFast toNext initialState))
        (loopHelp initialState toNext)


loopFast :
    (state -> Decoder context error (Step state a))
    -> state
    -> Decode.Decoder a
loopFast toNext initial =
    Decode.loop initial
        (\loopState ->
            case toNext loopState of
                Decoder (Just dec) _ ->
                    Decode.map
                        (\step ->
                            case step of
                                Loop s ->
                                    Decode.Loop s

                                Done a ->
                                    Decode.Done a
                        )
                        dec

                Decoder Nothing _ ->
                    Decode.fail
        )


loopHelp :
    state
    -> (state -> Decoder context error (Step state a))
    -> State
    -> DecodeResult context error a
loopHelp loopState toNext state =
    let
        (Decoder _ slow) =
            toNext loopState
    in
    case slow state of
        Good (Loop newLoopState) newState ->
            loopHelp newLoopState toNext newState

        Good (Done v) newState ->
            Good v newState

        Bad e ->
            Bad e


{-| Decode exactly `count` copies of the same decoder into a list.

If the inner decoder has a fast path, `repeat` preserves it by composing
a single `Decode.loop` decoder.

-}
repeat : Decoder context error value -> Int -> Decoder context error (List value)
repeat (Decoder maybeDec slow) nTimes =
    Decoder
        (Maybe.map (\dec -> repeatFast dec nTimes) maybeDec)
        (repeatHelp slow nTimes [])


repeatFast : Decode.Decoder value -> Int -> Decode.Decoder (List value)
repeatFast dec nTimes =
    Decode.loop ( nTimes, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                Decode.map (\v -> Decode.Loop ( remaining - 1, v :: acc )) dec
        )


repeatHelp :
    (State -> DecodeResult context error value)
    -> Int
    -> List value
    -> State
    -> DecodeResult context error (List value)
repeatHelp slow remaining acc state =
    if remaining <= 0 then
        Good (List.reverse acc) state

    else
        case slow state of
            Good v newState ->
                repeatHelp slow (remaining - 1) (v :: acc) newState

            Bad e ->
                Bad e



-- RANDOM ACCESS


{-| An opaque position in the input, obtained via [`position`](#position).
-}
type Position
    = Position Int


{-| Produce the current byte offset in the input.

This decoder has no fast path — using it will cause `decode` to fall back to
the slow path.

    import Bytes.Decoder as BD

    BD.decode BD.position (fromList [ 1, 2, 3 ])
    --> Ok BD.startOfInput

    BD.decode
        (BD.unsignedInt8
            |> BD.andThen (\_ -> BD.position)
        )
        (fromList [ 1, 2, 3 ])
        |> Result.map (\\p -> p == BD.startOfInput)
    --> Ok False

-}
position : Decoder context error Position
position =
    Decoder Nothing (\state -> Good (Position state.offset) state)


{-| The position at the very start of the input (offset 0).

Useful as the `relativeTo` argument in [`randomAccess`](#randomAccess).

-}
startOfInput : Position
startOfInput =
    Position 0


{-| Read data at an arbitrary offset, then resume where you left off.

This decoder has no fast path.

As an example, consider data laid out as:

  - A byte giving the length of a string
  - A byte giving the absolute offset to the string
  - Another byte we also need

<!-- -->

    import Bytes exposing (Bytes)
    import Bytes.Decoder as BD exposing (Decoder)
    import Bytes.Encode as E

    input : Bytes
    input =
        [ E.unsignedInt8 5
        , E.unsignedInt8 15
        , E.unsignedInt8 6
        , E.string (String.repeat 12 "\u{0000}")
        , E.string "hello"
        ]
            |> E.sequence
            |> E.encode

    stringAtOffset : Decoder c e String
    stringAtOffset =
        BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8
            |> BD.andThen
                (\( len, offset ) ->
                    BD.randomAccess
                        { offset = offset, relativeTo = BD.startOfInput }
                        (BD.string len)
                )

    BD.decode stringAtOffset input
    --> Ok "hello"

Parsing continues at the sequential position after `randomAccess` returns:

    final : Decoder c e { string : String, number : Int }
    final =
        BD.succeed (\s n -> { string = s, number = n })
            |> BD.keep stringAtOffset
            |> BD.keep BD.unsignedInt8

    BD.decode final input
    --> Ok { string = "hello", number = 6 }

-}
randomAccess :
    { offset : Int, relativeTo : Position }
    -> Decoder context error value
    -> Decoder context error value
randomAccess config (Decoder _ slow) =
    Decoder Nothing
        (\state ->
            let
                (Position start) =
                    config.relativeTo
            in
            case slow { offset = start + config.offset, input = state.input } of
                Good v _ ->
                    Good v state

                Bad e ->
                    Bad e
        )



-- PRIMITIVES


{-| Decode one byte as an unsigned integer from 0 to 255.
-}
unsignedInt8 : Decoder context error Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


{-| Decode one byte as a signed integer from -128 to 127.
-}
signedInt8 : Decoder context error Int
signedInt8 =
    fromDecoder Decode.signedInt8 1


{-| Decode two bytes as an unsigned integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Decoder context error Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


{-| Decode two bytes as a signed integer from -32768 to 32767.
-}
signedInt16 : Bytes.Endianness -> Decoder context error Int
signedInt16 bo =
    fromDecoder (Decode.signedInt16 bo) 2


{-| Decode four bytes as an unsigned integer from 0 to 4294967295.
-}
unsignedInt32 : Bytes.Endianness -> Decoder context error Int
unsignedInt32 bo =
    fromDecoder (Decode.unsignedInt32 bo) 4


{-| Decode four bytes as a signed integer from -2147483648 to 2147483647.
-}
signedInt32 : Bytes.Endianness -> Decoder context error Int
signedInt32 bo =
    fromDecoder (Decode.signedInt32 bo) 4


{-| Decode two bytes as a 16-bit float.
-}
float16 : Bytes.Endianness -> Decoder context error Float
float16 bo =
    fromDecoder (Bytes.Floating.Decode.float16 bo) 2


{-| Decode four bytes as a 32-bit float.
-}
float32 : Bytes.Endianness -> Decoder context error Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


{-| Decode eight bytes as a 64-bit float.
-}
float64 : Bytes.Endianness -> Decoder context error Float
float64 bo =
    fromDecoder (Decode.float64 bo) 8


{-| Decode exactly `n` bytes.
-}
bytes : Int -> Decoder context error Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


{-| Decode exactly `n` bytes as a UTF-8 string.
-}
string : Int -> Decoder context error String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount



-- INTERNAL


fromDecoder : Decode.Decoder v -> Int -> Decoder context error v
fromDecoder dec byteLength =
    Decoder
        (Just dec)
        (\state ->
            let
                combined : Decode.Decoder v
                combined =
                    Decode.map2 (\_ v -> v) (Decode.bytes state.offset) dec
            in
            case Decode.decode combined state.input of
                Just res ->
                    Good res { offset = state.offset + byteLength, input = state.input }

                Nothing ->
                    Bad (OutOfBounds { at = state.offset, bytes = byteLength })
        )
