module Bytes.Decoder exposing
    ( Decoder
    , Error(..)
    , decode
    , unsignedInt8, unsignedInt16, unsignedInt32
    , signedInt8, signedInt16, signedInt32
    , float32, float64
    , bytes, string
    , succeed, fail
    , map, map2, map3, map4, map5
    , andThen
    , keep, ignore, skip
    , oneOf
    , Step(..), loop, repeat
    , mapError, inContext
    , offsetAt
    )

{-| Fast bytes decoder with error reporting and `oneOf` branching.

Uses a dual-path architecture:

  - **Fast path**: raw `elm/bytes` decoder composition for purely applicative
    chains (`map2`–`map5`, `keep`, `ignore`, `repeat`). At the top-level
    `decode` call, this is zero overhead vs raw `elm/bytes`.
  - **Slow path**: state-passing function with per-primitive skip-to-offset.
    Used for `andThen`, `oneOf`, `loop`, and as fallback for error reporting.


# Running

@docs Decoder, Error, decode


# Primitives

@docs unsignedInt8, unsignedInt16, unsignedInt32
@docs signedInt8, signedInt16, signedInt32
@docs float32, float64
@docs bytes, string


# Static

@docs succeed, fail


# Mapping

@docs map, map2, map3, map4, map5


# Chaining

@docs andThen


# Pipeline

@docs keep, ignore, skip


# Branching

@docs oneOf


# Looping

@docs Step, loop, repeat


# Errors

@docs mapError, inContext


# Query

@docs offsetAt

-}

import Bytes exposing (Bytes, Endianness)
import Bytes.Decode as D
import Bytes.Encode as E



-- TYPES


{-| A decoder that reads binary data and produces either a value or an error.

Internally, decoders may carry a fast-path raw `elm/bytes` decoder for
applicative composition, or just a slow-path function with error tracking.
-}
type Decoder error value
    = Fast (D.Decoder value) Int (Bytes -> Int -> Result (Error error) ( Int, value ))
    | Slow (Bytes -> Int -> Result (Error error) ( Int, value ))


{-| Errors that can occur during decoding.

  - `OutOfBounds` — tried to read past the end of input.
  - `CustomError` — user-defined error at a given byte offset.
  - `OneOfErrors` — all alternatives in a `oneOf` failed; collects each error.
  - `InContext` — wraps an inner error with a context label.

-}
type Error error
    = OutOfBounds { offset : Int, bytesNeeded : Int }
    | CustomError Int error
    | OneOfErrors Int (List (Error error))
    | InContext String (Error error)


{-| A loop step: either continue with new state or finish with a value.
-}
type Step state a
    = Loop state
    | Done a



-- RUNNING


{-| Run a decoder on some bytes.

For purely applicative decoders (built only with `map2`–`map5`, `keep`,
`ignore`, `repeat` over primitives), this takes the fast path — a single
`elm/bytes` decode call with zero wrapper overhead.

On failure, the slow path re-runs for detailed error info.

-}
decode : Decoder error value -> Bytes -> Result (Error error) value
decode decoder input =
    case decoder of
        Fast fast width slow ->
            if width <= Bytes.width input then
                case D.decode fast input of
                    Just v ->
                        Ok v

                    Nothing ->
                        slow input 0 |> Result.map Tuple.second

            else
                slow input 0 |> Result.map Tuple.second

        Slow slow ->
            slow input 0 |> Result.map Tuple.second



-- PRIMITIVES


{-| Decode one byte as an unsigned integer from 0 to 255.
-}
unsignedInt8 : Decoder error Int
unsignedInt8 =
    primitive D.unsignedInt8 1


{-| Decode two bytes as an unsigned integer.
-}
unsignedInt16 : Endianness -> Decoder error Int
unsignedInt16 e =
    primitive (D.unsignedInt16 e) 2


{-| Decode four bytes as an unsigned integer.
-}
unsignedInt32 : Endianness -> Decoder error Int
unsignedInt32 e =
    primitive (D.unsignedInt32 e) 4


{-| Decode one byte as a signed integer from -128 to 127.
-}
signedInt8 : Decoder error Int
signedInt8 =
    primitive D.signedInt8 1


{-| Decode two bytes as a signed integer.
-}
signedInt16 : Endianness -> Decoder error Int
signedInt16 e =
    primitive (D.signedInt16 e) 2


{-| Decode four bytes as a signed integer.
-}
signedInt32 : Endianness -> Decoder error Int
signedInt32 e =
    primitive (D.signedInt32 e) 4


{-| Decode four bytes as a 32-bit float.
-}
float32 : Endianness -> Decoder error Float
float32 e =
    primitive (D.float32 e) 4


{-| Decode eight bytes as a 64-bit float.
-}
float64 : Endianness -> Decoder error Float
float64 e =
    primitive (D.float64 e) 8


{-| Decode exactly `n` bytes.
-}
bytes : Int -> Decoder error Bytes
bytes n =
    primitive (D.bytes n) n


{-| Decode exactly `n` bytes as a UTF-8 string.
-}
string : Int -> Decoder error String
string n =
    primitive (D.string n) n



-- STATIC


{-| A decoder that always succeeds with the given value, consuming zero bytes.
-}
succeed : value -> Decoder error value
succeed v =
    Fast (D.succeed v) 0 (\_ offset -> Ok ( offset, v ))


{-| A decoder that always fails with the given error at the current offset.
-}
fail : error -> Decoder error a
fail e =
    Slow (\_ offset -> Err (CustomError offset e))



-- MAPPING


{-| Transform the value produced by a decoder.
-}
map : (a -> b) -> Decoder error a -> Decoder error b
map f decoder =
    case decoder of
        Fast fast width slow ->
            Fast (D.map f fast) width (slowMap f slow)

        Slow slow ->
            Slow (slowMap f slow)


{-| Combine two sequential decoders.

If both operands have a fast path, the result preserves it.

-}
map2 : (a -> b -> c) -> Decoder error a -> Decoder error b -> Decoder error c
map2 f da db =
    let
        slow =
            \input offset ->
                case runAt da input offset of
                    Err e ->
                        Err e

                    Ok ( o2, a_ ) ->
                        case runAt db input o2 of
                            Err e ->
                                Err e

                            Ok ( o3, b_ ) ->
                                Ok ( o3, f a_ b_ )
    in
    case ( da, db ) of
        ( Fast fa wa _, Fast fb wb _ ) ->
            Fast (D.map2 f fa fb) (wa + wb) slow

        _ ->
            Slow slow


{-| Combine three sequential decoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Decoder error a
    -> Decoder error b
    -> Decoder error c
    -> Decoder error d
map3 f da db dc =
    let
        slow =
            \input offset ->
                case runAt da input offset of
                    Err e ->
                        Err e

                    Ok ( o2, a_ ) ->
                        case runAt db input o2 of
                            Err e ->
                                Err e

                            Ok ( o3, b_ ) ->
                                case runAt dc input o3 of
                                    Err e ->
                                        Err e

                                    Ok ( o4, c_ ) ->
                                        Ok ( o4, f a_ b_ c_ )
    in
    case ( da, db, dc ) of
        ( Fast fa wa _, Fast fb wb _, Fast fc wc _ ) ->
            Fast (D.map3 f fa fb fc) (wa + wb + wc) slow

        _ ->
            Slow slow


{-| Combine four sequential decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder error a
    -> Decoder error b
    -> Decoder error c
    -> Decoder error d
    -> Decoder error e
map4 f da db dc dd =
    let
        slow =
            \input offset ->
                case runAt da input offset of
                    Err e ->
                        Err e

                    Ok ( o2, a_ ) ->
                        case runAt db input o2 of
                            Err e ->
                                Err e

                            Ok ( o3, b_ ) ->
                                case runAt dc input o3 of
                                    Err e ->
                                        Err e

                                    Ok ( o4, c_ ) ->
                                        case runAt dd input o4 of
                                            Err e ->
                                                Err e

                                            Ok ( o5, d_ ) ->
                                                Ok ( o5, f a_ b_ c_ d_ )
    in
    case ( da, db ) of
        ( Fast fa wa _, Fast fb wb _ ) ->
            case ( dc, dd ) of
                ( Fast fc wc _, Fast fd wd _ ) ->
                    Fast (D.map4 f fa fb fc fd) (wa + wb + wc + wd) slow

                _ ->
                    Slow slow

        _ ->
            Slow slow


{-| Combine five sequential decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder error a
    -> Decoder error b
    -> Decoder error c
    -> Decoder error d
    -> Decoder error e
    -> Decoder error f
map5 f da db dc dd de =
    let
        slow =
            \input offset ->
                case runAt da input offset of
                    Err e ->
                        Err e

                    Ok ( o2, a_ ) ->
                        case runAt db input o2 of
                            Err e ->
                                Err e

                            Ok ( o3, b_ ) ->
                                case runAt dc input o3 of
                                    Err e ->
                                        Err e

                                    Ok ( o4, c_ ) ->
                                        case runAt dd input o4 of
                                            Err e ->
                                                Err e

                                            Ok ( o5, d_ ) ->
                                                case runAt de input o5 of
                                                    Err e ->
                                                        Err e

                                                    Ok ( o6, e_ ) ->
                                                        Ok ( o6, f a_ b_ c_ d_ e_ )
    in
    case ( da, db ) of
        ( Fast fa wa _, Fast fb wb _ ) ->
            case ( dc, dd ) of
                ( Fast fc wc _, Fast fd wd _ ) ->
                    case de of
                        Fast fe we _ ->
                            Fast (D.map5 f fa fb fc fd fe) (wa + wb + wc + wd + we) slow

                        _ ->
                            Slow slow

                _ ->
                    Slow slow

        _ ->
            Slow slow



-- CHAINING


{-| Create a decoder whose next step depends on a previously decoded value.

**Note:** `andThen` always uses the slow path. Prefer `map2`–`map5` or
`keep`/`ignore` when the decoding structure is known statically.

-}
andThen : (a -> Decoder error b) -> Decoder error a -> Decoder error b
andThen callback da =
    Slow
        (\input offset ->
            case runAt da input offset of
                Err e ->
                    Err e

                Ok ( newOffset, a_ ) ->
                    runAt (callback a_) input newOffset
        )



-- PIPELINE


{-| Decode a value and apply it to the function held by the previous decoder.

    import Bytes exposing (Endianness(..))
    import Bytes.Decoder as BD

    type alias Point =
        { x : Float, y : Float }

    pointDecoder : BD.Decoder error Point
    pointDecoder =
        BD.succeed Point
            |> BD.keep (BD.float32 BE)
            |> BD.keep (BD.float32 BE)

-}
keep : Decoder error a -> Decoder error (a -> b) -> Decoder error b
keep da df =
    map2 (\g a_ -> g a_) df da


{-| Run a decoder but discard its result, keeping the previous value.
-}
ignore : Decoder error x -> Decoder error a -> Decoder error a
ignore dx da =
    map2 (\a_ _ -> a_) da dx


{-| Skip `n` bytes, then continue with the given decoder.
-}
skip : Int -> Decoder error a -> Decoder error a
skip n da =
    map2 (\_ a_ -> a_) (bytes n) da



-- BRANCHING


{-| Try each decoder in order, returning the first success.

For alternatives with a fast path, `oneOf` tries the fast `elm/bytes` decoder
on a pre-sliced input — one byte-slice shared across all alternatives — avoiding
per-primitive skip-to-offset overhead.

Collects all errors on failure for diagnostic purposes.

-}
oneOf : List (Decoder error value) -> Decoder error value
oneOf options =
    Slow
        (\input offset ->
            let
                sliced =
                    sliceFrom offset input
            in
            oneOfHelper sliced input offset options []
        )



-- LOOPING


{-| Decode in a loop. Each iteration produces either `Loop` (continue with new
state) or `Done` (finish with a value).
-}
loop : state -> (state -> Decoder error (Step state a)) -> Decoder error a
loop init step =
    Slow (\input offset -> loopHelper init step input offset)


{-| Decode exactly `count` copies of the same decoder into a list.

If the inner decoder has a fast path, `repeat` preserves it by composing
a single `elm/bytes` loop decoder.

-}
repeat : Decoder error value -> Int -> Decoder error (List value)
repeat decoder count =
    if count <= 0 then
        succeed []

    else
        let
            slow =
                \input offset ->
                    repeatHelper decoder count input offset []
        in
        case decoder of
            Fast fast width _ ->
                Fast
                    (D.loop ( count, [] )
                        (\( remaining, acc ) ->
                            if remaining <= 0 then
                                D.succeed (D.Done (List.reverse acc))

                            else
                                D.map (\v -> D.Loop ( remaining - 1, v :: acc )) fast
                        )
                    )
                    (width * count)
                    slow

            _ ->
                Slow slow



-- ERRORS


{-| Transform the error type.
-}
mapError : (e1 -> e2) -> Decoder e1 a -> Decoder e2 a
mapError f decoder =
    case decoder of
        Fast fast width slow ->
            Fast fast width (\input offset -> slow input offset |> Result.mapError (transformError f))

        Slow slow ->
            Slow (\input offset -> slow input offset |> Result.mapError (transformError f))


{-| Add context to errors. On the fast (success) path, this is free.
-}
inContext : String -> Decoder error value -> Decoder error value
inContext label decoder =
    case decoder of
        Fast fast width slow ->
            Fast fast width (wrapContext label slow)

        Slow slow ->
            Slow (wrapContext label slow)


{-| Get the current byte offset without consuming any bytes.

**Note:** this disables the fast path for any composition that includes it.

-}
offsetAt : Decoder error Int
offsetAt =
    Slow (\_ offset -> Ok ( offset, offset ))



-- INTERNAL: primitive construction


primitive : D.Decoder a -> Int -> Decoder error a
primitive decoder byteWidth =
    Fast decoder byteWidth
        (\input offset ->
            if offset + byteWidth > Bytes.width input then
                Err (OutOfBounds { offset = offset, bytesNeeded = byteWidth })

            else
                case D.decode (skipThen offset decoder) input of
                    Just v ->
                        Ok ( offset + byteWidth, v )

                    Nothing ->
                        Err (OutOfBounds { offset = offset, bytesNeeded = byteWidth })
        )


skipThen : Int -> D.Decoder a -> D.Decoder a
skipThen offset decoder =
    if offset == 0 then
        decoder

    else
        D.bytes offset |> D.andThen (\_ -> decoder)



-- INTERNAL: running decoders


{-| Run a decoder's slow path at a given offset.
Used for sequential composition in andThen, map2, etc.
-}
runAt : Decoder error value -> Bytes -> Int -> Result (Error error) ( Int, value )
runAt decoder input offset =
    case decoder of
        Fast _ _ slow ->
            slow input offset

        Slow slow ->
            slow input offset



-- INTERNAL: oneOf helpers


oneOfHelper :
    Bytes
    -> Bytes
    -> Int
    -> List (Decoder error value)
    -> List (Error error)
    -> Result (Error error) ( Int, value )
oneOfHelper sliced originalInput offset options errors =
    case options of
        [] ->
            Err (OneOfErrors offset (List.reverse errors))

        d :: rest ->
            case oneOfTry sliced d originalInput offset of
                Ok result ->
                    Ok result

                Err e ->
                    oneOfHelper sliced originalInput offset rest (e :: errors)


{-| Try a decoder in a oneOf context. For Fast decoders, try the fast path
on the pre-sliced input first (avoids per-primitive skip overhead).
-}
oneOfTry :
    Bytes
    -> Decoder error value
    -> Bytes
    -> Int
    -> Result (Error error) ( Int, value )
oneOfTry sliced decoder originalInput offset =
    case decoder of
        Fast fast width slow ->
            if width <= Bytes.width sliced then
                case D.decode fast sliced of
                    Just v ->
                        Ok ( offset + width, v )

                    Nothing ->
                        slow originalInput offset

            else
                slow originalInput offset

        Slow slow ->
            slow originalInput offset


sliceFrom : Int -> Bytes -> Bytes
sliceFrom offset input =
    if offset <= 0 then
        input

    else
        let
            remaining =
                Bytes.width input - offset
        in
        if remaining <= 0 then
            emptyBytes

        else
            case D.decode (D.bytes offset |> D.andThen (\_ -> D.bytes remaining)) input of
                Just sliced ->
                    sliced

                Nothing ->
                    emptyBytes


emptyBytes : Bytes
emptyBytes =
    E.encode (E.sequence [])



-- INTERNAL: loop helpers


loopHelper :
    state
    -> (state -> Decoder error (Step state a))
    -> Bytes
    -> Int
    -> Result (Error error) ( Int, a )
loopHelper state step input offset =
    case runAt (step state) input offset of
        Err e ->
            Err e

        Ok ( newOffset, stepResult ) ->
            case stepResult of
                Loop newState ->
                    loopHelper newState step input newOffset

                Done a_ ->
                    Ok ( newOffset, a_ )


repeatHelper :
    Decoder error value
    -> Int
    -> Bytes
    -> Int
    -> List value
    -> Result (Error error) ( Int, List value )
repeatHelper decoder remaining input offset acc =
    if remaining <= 0 then
        Ok ( offset, List.reverse acc )

    else
        case runAt decoder input offset of
            Err e ->
                Err e

            Ok ( newOffset, v ) ->
                repeatHelper decoder (remaining - 1) input newOffset (v :: acc)



-- INTERNAL: error helpers


slowMap :
    (a -> b)
    -> (Bytes -> Int -> Result (Error error) ( Int, a ))
    -> Bytes
    -> Int
    -> Result (Error error) ( Int, b )
slowMap f slow input offset =
    case slow input offset of
        Ok ( o, a_ ) ->
            Ok ( o, f a_ )

        Err e ->
            Err e


wrapContext :
    String
    -> (Bytes -> Int -> Result (Error error) ( Int, value ))
    -> Bytes
    -> Int
    -> Result (Error error) ( Int, value )
wrapContext label slow input offset =
    case slow input offset of
        Ok result ->
            Ok result

        Err e ->
            Err (InContext label e)


transformError : (e1 -> e2) -> Error e1 -> Error e2
transformError f error =
    case error of
        OutOfBounds info ->
            OutOfBounds info

        CustomError offset e ->
            CustomError offset (f e)

        OneOfErrors offset errors ->
            OneOfErrors offset (List.map (transformError f) errors)

        InContext label inner ->
            InContext label (transformError f inner)
