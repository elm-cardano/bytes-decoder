module Bench exposing
    ( raw_map5
    , raw_loop_10
    , raw_loop_100
    , raw_loop_1000
    , bd_map5_fast
    , bd_keep5_fast
    , bd_repeat_10
    , bd_repeat_100
    , bd_repeat_1000
    , bd_loop_10
    , bd_loop_100
    , bd_loop_1000
    , bd_andThen_5
    , bd_oneOf_first
    , bd_oneOf_last
    )

{-| Benchmarks comparing raw elm/bytes vs Bytes.Decoder.

## Sequential (applicative) — map5, 5 fields

    elm-bench -f Bench.raw_map5 -f Bench.bd_map5_fast -f Bench.bd_keep5_fast "()"

## Repeat / loop — N float64s

    elm-bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 "()"
    elm-bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 "()"

## andThen (slow path) — 5 fields via chained andThen

    elm-bench -f Bench.raw_map5 -f Bench.bd_andThen_5 "()"

## oneOf branching

    elm-bench -f Bench.bd_oneOf_first -f Bench.bd_oneOf_last "()"

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Decoder as BD
import Bytes.Encode as E



-- TEST DATA


{-| 5 unsigned bytes: [1, 2, 3, 4, 5]
-}
data5 : Bytes
data5 =
    E.encode
        (E.sequence
            [ E.unsignedInt8 1
            , E.unsignedInt8 2
            , E.unsignedInt8 3
            , E.unsignedInt8 4
            , E.unsignedInt8 5
            ]
        )


{-| N big-endian float64 values.
-}
makeFloats : Int -> Bytes
makeFloats n =
    E.encode (E.sequence (List.repeat n (E.float64 BE 3.14159)))


floats10 : Bytes
floats10 =
    makeFloats 10


floats100 : Bytes
floats100 =
    makeFloats 100


floats1000 : Bytes
floats1000 =
    makeFloats 1000


{-| oneOf test data: tag byte 2 followed by 4 bytes.
Tag 2 matches the third alternative.
-}
oneOfData : Bytes
oneOfData =
    E.encode
        (E.sequence
            [ E.unsignedInt8 2
            , E.unsignedInt8 10
            , E.unsignedInt8 20
            , E.unsignedInt8 30
            , E.unsignedInt8 40
            ]
        )



-- RAW elm/bytes BENCHMARKS


type alias Record5 =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


raw_map5 : () -> Maybe Record5
raw_map5 () =
    D.decode (D.map5 Record5 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8) data5


rawLoopFloat64 : Int -> D.Decoder (List Float)
rawLoopFloat64 n =
    D.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\v -> D.Loop ( remaining - 1, v :: acc )) (D.float64 BE)
        )


raw_loop_10 : () -> Maybe (List Float)
raw_loop_10 () =
    D.decode (rawLoopFloat64 10) floats10


raw_loop_100 : () -> Maybe (List Float)
raw_loop_100 () =
    D.decode (rawLoopFloat64 100) floats100


raw_loop_1000 : () -> Maybe (List Float)
raw_loop_1000 () =
    D.decode (rawLoopFloat64 1000) floats1000



-- Bytes.Decoder BENCHMARKS: fast path (applicative)


bd_map5_fast : () -> Maybe Record5
bd_map5_fast () =
    BD.decode
        (BD.map5 Record5
            BD.unsignedInt8
            BD.unsignedInt8
            BD.unsignedInt8
            BD.unsignedInt8
            BD.unsignedInt8
        )
        data5
        |> Result.toMaybe


bd_keep5_fast : () -> Maybe Record5
bd_keep5_fast () =
    BD.decode
        (BD.succeed Record5
            |> BD.keep BD.unsignedInt8
            |> BD.keep BD.unsignedInt8
            |> BD.keep BD.unsignedInt8
            |> BD.keep BD.unsignedInt8
            |> BD.keep BD.unsignedInt8
        )
        data5
        |> Result.toMaybe



-- Bytes.Decoder BENCHMARKS: repeat (fast path via D.loop)


bd_repeat_10 : () -> Result (BD.Error e) (List Float)
bd_repeat_10 () =
    BD.decode (BD.repeat (BD.float64 BE) 10) floats10


bd_repeat_100 : () -> Result (BD.Error e) (List Float)
bd_repeat_100 () =
    BD.decode (BD.repeat (BD.float64 BE) 100) floats100


bd_repeat_1000 : () -> Result (BD.Error e) (List Float)
bd_repeat_1000 () =
    BD.decode (BD.repeat (BD.float64 BE) 1000) floats1000



-- Bytes.Decoder BENCHMARKS: loop (slow path)


bdLoopFloat64 : Int -> BD.Decoder e (List Float)
bdLoopFloat64 n =
    BD.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) (BD.float64 BE)
        )


bd_loop_10 : () -> Result (BD.Error e) (List Float)
bd_loop_10 () =
    BD.decode (bdLoopFloat64 10) floats10


bd_loop_100 : () -> Result (BD.Error e) (List Float)
bd_loop_100 () =
    BD.decode (bdLoopFloat64 100) floats100


bd_loop_1000 : () -> Result (BD.Error e) (List Float)
bd_loop_1000 () =
    BD.decode (bdLoopFloat64 1000) floats1000



-- Bytes.Decoder BENCHMARKS: andThen (slow path)


bd_andThen_5 : () -> Result (BD.Error e) Record5
bd_andThen_5 () =
    BD.decode
        (BD.unsignedInt8
            |> BD.andThen
                (\a ->
                    BD.unsignedInt8
                        |> BD.andThen
                            (\b ->
                                BD.unsignedInt8
                                    |> BD.andThen
                                        (\c ->
                                            BD.unsignedInt8
                                                |> BD.andThen
                                                    (\d ->
                                                        BD.map (\e -> Record5 a b c d e)
                                                            BD.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5



-- Bytes.Decoder BENCHMARKS: oneOf branching


type OneOfResult
    = Tag0 Int
    | Tag1 Int Int
    | Tag2 Int Int Int Int


bdOneOfDecoder : BD.Decoder String OneOfResult
bdOneOfDecoder =
    BD.oneOf
        [ BD.map2 (\_ v -> Tag0 v)
            (BD.unsignedInt8 |> BD.andThen (\t -> if t == 0 then BD.succeed t else BD.fail "not 0"))
            BD.unsignedInt8
        , BD.map3 (\_ a b -> Tag1 a b)
            (BD.unsignedInt8 |> BD.andThen (\t -> if t == 1 then BD.succeed t else BD.fail "not 1"))
            BD.unsignedInt8
            BD.unsignedInt8
        , BD.map5 (\_ a b c d -> Tag2 a b c d)
            (BD.unsignedInt8 |> BD.andThen (\t -> if t == 2 then BD.succeed t else BD.fail "not 2"))
            BD.unsignedInt8
            BD.unsignedInt8
            BD.unsignedInt8
            BD.unsignedInt8
        ]


{-| oneOf with data matching tag 0 (first alternative).
-}
bd_oneOf_first : () -> Result (BD.Error String) OneOfResult
bd_oneOf_first () =
    let
        firstData =
            E.encode
                (E.sequence
                    [ E.unsignedInt8 0
                    , E.unsignedInt8 42
                    ]
                )
    in
    BD.decode bdOneOfDecoder firstData


{-| oneOf with data matching tag 2 (third/last alternative).
-}
bd_oneOf_last : () -> Result (BD.Error String) OneOfResult
bd_oneOf_last () =
    BD.decode bdOneOfDecoder oneOfData
