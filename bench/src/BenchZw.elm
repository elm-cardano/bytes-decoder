module BenchZw exposing
    ( zw_map2
    , zw_keep5
    , zw_map5
    , zw_repeat_100
    , zw_loop_10
    , zw_loop_100
    , zw_loop_1000
    , zw_andThen_5
    , zw_oneOf_first
    , zw_oneOf_last
    , zw_packet
    , zw_message
    , zw_tagged50
    , p2_map2
    , p2_keep5
    , p2_map5
    , p2_repeat_100
    , p2_loop_10
    , p2_loop_100
    , p2_loop_1000
    , p2_andThen_5
    , p2_oneOf_first
    , p2_oneOf_last
    , p2_packet
    , p2_message
    , p2_tagged50
    , p3_map2
    , p3_keep5
    , p3_map5
    , p3_repeat_100
    , p3_loop_10
    , p3_loop_100
    , p3_loop_1000
    , p3_andThen_5
    , p3_oneOf_first
    , p3_oneOf_last
    , p3_packet
    , p3_message
    , p3_tagged50
    )

{-| Head-to-head benchmarks: zwilias/elm-bytes-parser (ZW) vs Bytes.Parser2 (P2).

    elm-bench -f BenchZw.zw_map2 -f BenchZw.p2_map2 "()"
    elm-bench -f BenchZw.zw_keep5 -f BenchZw.p2_keep5 "()"
    elm-bench -f BenchZw.zw_map5 -f BenchZw.p2_map5 "()"
    elm-bench -f BenchZw.zw_repeat_100 -f BenchZw.p2_repeat_100 "()"
    elm-bench -f BenchZw.zw_loop_100 -f BenchZw.p2_loop_100 "()"
    elm-bench -f BenchZw.zw_loop_1000 -f BenchZw.p2_loop_1000 "()"
    elm-bench -f BenchZw.zw_andThen_5 -f BenchZw.p2_andThen_5 "()"
    elm-bench -f BenchZw.zw_oneOf_first -f BenchZw.p2_oneOf_first "()"
    elm-bench -f BenchZw.zw_oneOf_last -f BenchZw.p2_oneOf_last "()"
    elm-bench -f BenchZw.zw_packet -f BenchZw.p2_packet "()"
    elm-bench -f BenchZw.zw_message -f BenchZw.p2_message "()"
    elm-bench -f BenchZw.zw_tagged50 -f BenchZw.p2_tagged50 "()"

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Parser as ZW
import Bytes.Parser2 as P2
import Bytes.Parser3 as P3



-- TEST DATA


data2 : Bytes
data2 =
    E.encode
        (E.sequence
            [ E.unsignedInt8 1
            , E.unsignedInt8 2
            ]
        )


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


oneOfFirstData : Bytes
oneOfFirstData =
    E.encode
        (E.sequence
            [ E.unsignedInt8 0
            , E.unsignedInt8 42
            ]
        )


oneOfLastData : Bytes
oneOfLastData =
    E.encode
        (E.sequence
            [ E.unsignedInt8 2
            , E.unsignedInt8 10
            , E.unsignedInt8 20
            , E.unsignedInt8 30
            , E.unsignedInt8 40
            ]
        )


packetData : Bytes
packetData =
    E.encode
        (E.sequence
            [ E.unsignedInt32 BE 0xDEADBEEF
            , E.unsignedInt16 BE 1
            , E.signedInt8 -1
            , E.unsignedInt8 0
            , E.float32 BE 1.5
            , E.float64 BE 3.14159
            , E.unsignedInt8 0xCA
            , E.unsignedInt8 0xFE
            , E.unsignedInt8 0xBA
            , E.unsignedInt8 0xBE
            , E.string "Elm"
            , E.unsignedInt8 0xFF
            , E.sequence (List.map (E.unsignedInt16 BE) (List.range 1 10))
            ]
        )


messageData : Bytes
messageData =
    E.encode
        (E.sequence
            [ E.unsignedInt32 BE 0xCAFEBABE
            , E.unsignedInt16 BE 1
            , E.signedInt8 -5
            , E.unsignedInt8 3
            , E.signedInt32 BE 100
            , E.float64 BE 1.1
            , E.signedInt32 BE 200
            , E.float64 BE 2.2
            , E.signedInt32 BE 300
            , E.float64 BE 3.3
            , E.float32 BE 99.9
            , E.signedInt16 BE -42
            , E.unsignedInt8 0xDE
            , E.unsignedInt8 0xAD
            , E.unsignedInt8 0xBE
            , E.unsignedInt8 0xEF
            , E.string "End"
            ]
        )


tagged50Data : Bytes
tagged50Data =
    E.encode
        (E.sequence
            (List.concatMap
                (\i ->
                    if modBy 2 i == 0 then
                        [ E.unsignedInt8 0, E.unsignedInt8 (i * 3) ]

                    else
                        [ E.unsignedInt8 2, E.unsignedInt8 i, E.unsignedInt8 (i * 2), E.unsignedInt8 (i + 10), E.unsignedInt8 (i * 5) ]
                )
                (List.range 0 49)
            )
        )



-- SHARED TYPES


type alias Record5 =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


type alias Packet =
    { magic : Int
    , version : Int
    , flags : Int
    , temperature : Float
    , timestamp : Float
    , payload : Bytes
    , label : String
    , samples : List Int
    }


type alias Message =
    { magic : Int
    , version : Int
    , priority : Int
    , fields : List ( Int, Float )
    , checksum : Float
    , code : Int
    , tag : Bytes
    , label : String
    }


type OneOfResult
    = Tag0 Int
    | Tag1 Int Int
    | Tag2 Int Int Int Int



-- ============================================================================
-- zwilias/elm-bytes-parser (ZW) — original
-- ============================================================================


zw_map2 : () -> Maybe ( Int, Int )
zw_map2 () =
    ZW.run
        (ZW.map2 Tuple.pair ZW.unsignedInt8 ZW.unsignedInt8)
        data2
        |> Result.toMaybe


zw_keep5 : () -> Maybe Record5
zw_keep5 () =
    ZW.run
        (ZW.succeed Record5
            |> ZW.keep ZW.unsignedInt8
            |> ZW.keep ZW.unsignedInt8
            |> ZW.keep ZW.unsignedInt8
            |> ZW.keep ZW.unsignedInt8
            |> ZW.keep ZW.unsignedInt8
        )
        data5
        |> Result.toMaybe


zw_map5 : () -> Maybe Record5
zw_map5 () =
    ZW.run
        (ZW.map5 Record5 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8)
        data5
        |> Result.toMaybe


zw_repeat_100 : () -> Maybe (List Float)
zw_repeat_100 () =
    ZW.run (ZW.repeat (ZW.float64 BE) 100) floats100 |> Result.toMaybe


zwLoopFloat64 : Int -> ZW.Parser c e (List Float)
zwLoopFloat64 n =
    ZW.loop
        (\( remaining, acc ) ->
            if remaining <= 0 then
                ZW.succeed (ZW.Done (List.reverse acc))

            else
                ZW.map (\v -> ZW.Loop ( remaining - 1, v :: acc )) (ZW.float64 BE)
        )
        ( n, [] )


zw_loop_10 : () -> Maybe (List Float)
zw_loop_10 () =
    ZW.run (zwLoopFloat64 10) floats10 |> Result.toMaybe


zw_loop_100 : () -> Maybe (List Float)
zw_loop_100 () =
    ZW.run (zwLoopFloat64 100) floats100 |> Result.toMaybe


zw_loop_1000 : () -> Maybe (List Float)
zw_loop_1000 () =
    ZW.run (zwLoopFloat64 1000) floats1000 |> Result.toMaybe


zw_andThen_5 : () -> Maybe Record5
zw_andThen_5 () =
    ZW.run
        (ZW.unsignedInt8
            |> ZW.andThen
                (\a ->
                    ZW.unsignedInt8
                        |> ZW.andThen
                            (\b ->
                                ZW.unsignedInt8
                                    |> ZW.andThen
                                        (\c ->
                                            ZW.unsignedInt8
                                                |> ZW.andThen
                                                    (\d ->
                                                        ZW.map (\e -> Record5 a b c d e) ZW.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5
        |> Result.toMaybe


zwOneOfDecoder : ZW.Parser c String OneOfResult
zwOneOfDecoder =
    ZW.oneOf
        [ ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 0 then ZW.map Tag0 ZW.unsignedInt8 else ZW.fail "not 0")
        , ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 1 then ZW.map2 Tag1 ZW.unsignedInt8 ZW.unsignedInt8 else ZW.fail "not 1")
        , ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 2 then ZW.map4 Tag2 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 else ZW.fail "not 2")
        ]


zw_oneOf_first : () -> Maybe OneOfResult
zw_oneOf_first () =
    ZW.run zwOneOfDecoder oneOfFirstData |> Result.toMaybe


zw_oneOf_last : () -> Maybe OneOfResult
zw_oneOf_last () =
    ZW.run zwOneOfDecoder oneOfLastData |> Result.toMaybe


zw_packet : () -> Maybe Packet
zw_packet () =
    ZW.run zwPacketDecoder packetData |> Result.toMaybe


zwPacketDecoder : ZW.Parser c e Packet
zwPacketDecoder =
    ZW.succeed Packet
        |> ZW.keep (ZW.unsignedInt32 BE)
        |> ZW.keep (ZW.unsignedInt16 BE)
        |> ZW.keep ZW.signedInt8
        |> ZW.skip 1
        |> ZW.keep (ZW.float32 BE)
        |> ZW.keep (ZW.float64 BE)
        |> ZW.keep (ZW.bytes 4)
        |> ZW.keep (ZW.string 3)
        |> ZW.ignore ZW.unsignedInt8
        |> ZW.keep (ZW.repeat (ZW.unsignedInt16 BE) 10)


zw_message : () -> Maybe Message
zw_message () =
    ZW.run zwMessageDecoder messageData |> Result.toMaybe


zwMessageDecoder : ZW.Parser c e Message
zwMessageDecoder =
    ZW.map3 (\magic version priority -> ( magic, version, priority ))
        (ZW.unsignedInt32 BE)
        (ZW.unsignedInt16 BE)
        ZW.signedInt8
        |> ZW.andThen
            (\( magic, version, priority ) ->
                ZW.unsignedInt8
                    |> ZW.andThen
                        (\fieldCount ->
                            ZW.loop
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        ZW.succeed (ZW.Done (List.reverse acc))

                                    else
                                        ZW.map2 (\k v -> ZW.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (ZW.signedInt32 BE)
                                            (ZW.float64 BE)
                                )
                                ( fieldCount, [] )
                                |> ZW.andThen
                                    (\fields ->
                                        ZW.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (ZW.float32 BE)
                                            (ZW.signedInt16 BE)
                                            (ZW.bytes 4)
                                            (ZW.string 3)
                                    )
                        )
            )


zw_tagged50 : () -> Maybe (List OneOfResult)
zw_tagged50 () =
    ZW.run
        (ZW.loop
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    ZW.succeed (ZW.Done (List.reverse acc))

                else
                    ZW.map (\v -> ZW.Loop ( remaining - 1, v :: acc )) zwOneOfDecoder
            )
            ( 50, [] )
        )
        tagged50Data
        |> Result.toMaybe



-- ============================================================================
-- Bytes.Parser2 (P2) — optimized copy
-- ============================================================================


p2_map2 : () -> Maybe ( Int, Int )
p2_map2 () =
    P2.run
        (P2.map2 Tuple.pair P2.unsignedInt8 P2.unsignedInt8)
        data2
        |> Result.toMaybe


p2_keep5 : () -> Maybe Record5
p2_keep5 () =
    P2.run
        (P2.succeed Record5
            |> P2.keep P2.unsignedInt8
            |> P2.keep P2.unsignedInt8
            |> P2.keep P2.unsignedInt8
            |> P2.keep P2.unsignedInt8
            |> P2.keep P2.unsignedInt8
        )
        data5
        |> Result.toMaybe


p2_map5 : () -> Maybe Record5
p2_map5 () =
    P2.run
        (P2.map5 Record5 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8)
        data5
        |> Result.toMaybe


p2_repeat_100 : () -> Maybe (List Float)
p2_repeat_100 () =
    P2.run (P2.repeat (P2.float64 BE) 100) floats100 |> Result.toMaybe


p2LoopFloat64 : Int -> P2.Parser c e (List Float)
p2LoopFloat64 n =
    P2.loop
        (\( remaining, acc ) ->
            if remaining <= 0 then
                P2.succeed (P2.Done (List.reverse acc))

            else
                P2.map (\v -> P2.Loop ( remaining - 1, v :: acc )) (P2.float64 BE)
        )
        ( n, [] )


p2_loop_10 : () -> Maybe (List Float)
p2_loop_10 () =
    P2.run (p2LoopFloat64 10) floats10 |> Result.toMaybe


p2_loop_100 : () -> Maybe (List Float)
p2_loop_100 () =
    P2.run (p2LoopFloat64 100) floats100 |> Result.toMaybe


p2_loop_1000 : () -> Maybe (List Float)
p2_loop_1000 () =
    P2.run (p2LoopFloat64 1000) floats1000 |> Result.toMaybe


p2_andThen_5 : () -> Maybe Record5
p2_andThen_5 () =
    P2.run
        (P2.unsignedInt8
            |> P2.andThen
                (\a ->
                    P2.unsignedInt8
                        |> P2.andThen
                            (\b ->
                                P2.unsignedInt8
                                    |> P2.andThen
                                        (\c ->
                                            P2.unsignedInt8
                                                |> P2.andThen
                                                    (\d ->
                                                        P2.map (\e -> Record5 a b c d e) P2.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5
        |> Result.toMaybe


p2OneOfDecoder : P2.Parser c String OneOfResult
p2OneOfDecoder =
    P2.oneOf
        [ P2.unsignedInt8 |> P2.andThen (\t -> if t == 0 then P2.map Tag0 P2.unsignedInt8 else P2.fail "not 0")
        , P2.unsignedInt8 |> P2.andThen (\t -> if t == 1 then P2.map2 Tag1 P2.unsignedInt8 P2.unsignedInt8 else P2.fail "not 1")
        , P2.unsignedInt8 |> P2.andThen (\t -> if t == 2 then P2.map4 Tag2 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 else P2.fail "not 2")
        ]


p2_oneOf_first : () -> Maybe OneOfResult
p2_oneOf_first () =
    P2.run p2OneOfDecoder oneOfFirstData |> Result.toMaybe


p2_oneOf_last : () -> Maybe OneOfResult
p2_oneOf_last () =
    P2.run p2OneOfDecoder oneOfLastData |> Result.toMaybe


p2_packet : () -> Maybe Packet
p2_packet () =
    P2.run p2PacketDecoder packetData |> Result.toMaybe


p2PacketDecoder : P2.Parser c e Packet
p2PacketDecoder =
    P2.succeed Packet
        |> P2.keep (P2.unsignedInt32 BE)
        |> P2.keep (P2.unsignedInt16 BE)
        |> P2.keep P2.signedInt8
        |> P2.skip 1
        |> P2.keep (P2.float32 BE)
        |> P2.keep (P2.float64 BE)
        |> P2.keep (P2.bytes 4)
        |> P2.keep (P2.string 3)
        |> P2.ignore P2.unsignedInt8
        |> P2.keep (P2.repeat (P2.unsignedInt16 BE) 10)


p2_message : () -> Maybe Message
p2_message () =
    P2.run p2MessageDecoder messageData |> Result.toMaybe


p2MessageDecoder : P2.Parser c e Message
p2MessageDecoder =
    P2.map3 (\magic version priority -> ( magic, version, priority ))
        (P2.unsignedInt32 BE)
        (P2.unsignedInt16 BE)
        P2.signedInt8
        |> P2.andThen
            (\( magic, version, priority ) ->
                P2.unsignedInt8
                    |> P2.andThen
                        (\fieldCount ->
                            P2.loop
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        P2.succeed (P2.Done (List.reverse acc))

                                    else
                                        P2.map2 (\k v -> P2.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (P2.signedInt32 BE)
                                            (P2.float64 BE)
                                )
                                ( fieldCount, [] )
                                |> P2.andThen
                                    (\fields ->
                                        P2.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (P2.float32 BE)
                                            (P2.signedInt16 BE)
                                            (P2.bytes 4)
                                            (P2.string 3)
                                    )
                        )
            )


p2_tagged50 : () -> Maybe (List OneOfResult)
p2_tagged50 () =
    P2.run
        (P2.loop
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    P2.succeed (P2.Done (List.reverse acc))

                else
                    P2.map (\v -> P2.Loop ( remaining - 1, v :: acc )) p2OneOfDecoder
            )
            ( 50, [] )
        )
        tagged50Data
        |> Result.toMaybe



-- ============================================================================
-- Bytes.Parser3 (P3) — inlined fromDecoder
-- ============================================================================


p3_map2 : () -> Maybe ( Int, Int )
p3_map2 () =
    P3.run
        (P3.map2 Tuple.pair P3.unsignedInt8 P3.unsignedInt8)
        data2
        |> Result.toMaybe


p3_keep5 : () -> Maybe Record5
p3_keep5 () =
    P3.run
        (P3.succeed Record5
            |> P3.keep P3.unsignedInt8
            |> P3.keep P3.unsignedInt8
            |> P3.keep P3.unsignedInt8
            |> P3.keep P3.unsignedInt8
            |> P3.keep P3.unsignedInt8
        )
        data5
        |> Result.toMaybe


p3_map5 : () -> Maybe Record5
p3_map5 () =
    P3.run
        (P3.map5 Record5 P3.unsignedInt8 P3.unsignedInt8 P3.unsignedInt8 P3.unsignedInt8 P3.unsignedInt8)
        data5
        |> Result.toMaybe


p3_repeat_100 : () -> Maybe (List Float)
p3_repeat_100 () =
    P3.run (P3.repeat (P3.float64 BE) 100) floats100 |> Result.toMaybe


p3LoopFloat64 : Int -> P3.Parser c e (List Float)
p3LoopFloat64 n =
    P3.loop
        (\( remaining, acc ) ->
            if remaining <= 0 then
                P3.succeed (P3.Done (List.reverse acc))

            else
                P3.map (\v -> P3.Loop ( remaining - 1, v :: acc )) (P3.float64 BE)
        )
        ( n, [] )


p3_loop_10 : () -> Maybe (List Float)
p3_loop_10 () =
    P3.run (p3LoopFloat64 10) floats10 |> Result.toMaybe


p3_loop_100 : () -> Maybe (List Float)
p3_loop_100 () =
    P3.run (p3LoopFloat64 100) floats100 |> Result.toMaybe


p3_loop_1000 : () -> Maybe (List Float)
p3_loop_1000 () =
    P3.run (p3LoopFloat64 1000) floats1000 |> Result.toMaybe


p3_andThen_5 : () -> Maybe Record5
p3_andThen_5 () =
    P3.run
        (P3.unsignedInt8
            |> P3.andThen
                (\a ->
                    P3.unsignedInt8
                        |> P3.andThen
                            (\b ->
                                P3.unsignedInt8
                                    |> P3.andThen
                                        (\c ->
                                            P3.unsignedInt8
                                                |> P3.andThen
                                                    (\d ->
                                                        P3.map (\e -> Record5 a b c d e) P3.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5
        |> Result.toMaybe


p3OneOfDecoder : P3.Parser c String OneOfResult
p3OneOfDecoder =
    P3.oneOf
        [ P3.unsignedInt8 |> P3.andThen (\t -> if t == 0 then P3.map Tag0 P3.unsignedInt8 else P3.fail "not 0")
        , P3.unsignedInt8 |> P3.andThen (\t -> if t == 1 then P3.map2 Tag1 P3.unsignedInt8 P3.unsignedInt8 else P3.fail "not 1")
        , P3.unsignedInt8 |> P3.andThen (\t -> if t == 2 then P3.map4 Tag2 P3.unsignedInt8 P3.unsignedInt8 P3.unsignedInt8 P3.unsignedInt8 else P3.fail "not 2")
        ]


p3_oneOf_first : () -> Maybe OneOfResult
p3_oneOf_first () =
    P3.run p3OneOfDecoder oneOfFirstData |> Result.toMaybe


p3_oneOf_last : () -> Maybe OneOfResult
p3_oneOf_last () =
    P3.run p3OneOfDecoder oneOfLastData |> Result.toMaybe


p3_packet : () -> Maybe Packet
p3_packet () =
    P3.run p3PacketDecoder packetData |> Result.toMaybe


p3PacketDecoder : P3.Parser c e Packet
p3PacketDecoder =
    P3.succeed Packet
        |> P3.keep (P3.unsignedInt32 BE)
        |> P3.keep (P3.unsignedInt16 BE)
        |> P3.keep P3.signedInt8
        |> P3.skip 1
        |> P3.keep (P3.float32 BE)
        |> P3.keep (P3.float64 BE)
        |> P3.keep (P3.bytes 4)
        |> P3.keep (P3.string 3)
        |> P3.ignore P3.unsignedInt8
        |> P3.keep (P3.repeat (P3.unsignedInt16 BE) 10)


p3_message : () -> Maybe Message
p3_message () =
    P3.run p3MessageDecoder messageData |> Result.toMaybe


p3MessageDecoder : P3.Parser c e Message
p3MessageDecoder =
    P3.map3 (\magic version priority -> ( magic, version, priority ))
        (P3.unsignedInt32 BE)
        (P3.unsignedInt16 BE)
        P3.signedInt8
        |> P3.andThen
            (\( magic, version, priority ) ->
                P3.unsignedInt8
                    |> P3.andThen
                        (\fieldCount ->
                            P3.loop
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        P3.succeed (P3.Done (List.reverse acc))

                                    else
                                        P3.map2 (\k v -> P3.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (P3.signedInt32 BE)
                                            (P3.float64 BE)
                                )
                                ( fieldCount, [] )
                                |> P3.andThen
                                    (\fields ->
                                        P3.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (P3.float32 BE)
                                            (P3.signedInt16 BE)
                                            (P3.bytes 4)
                                            (P3.string 3)
                                    )
                        )
            )


p3_tagged50 : () -> Maybe (List OneOfResult)
p3_tagged50 () =
    P3.run
        (P3.loop
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    P3.succeed (P3.Done (List.reverse acc))

                else
                    P3.map (\v -> P3.Loop ( remaining - 1, v :: acc )) p3OneOfDecoder
            )
            ( 50, [] )
        )
        tagged50Data
        |> Result.toMaybe
