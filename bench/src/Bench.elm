module Bench exposing
    ( bd_andThen_5
    , bd_keep5
    , bd_loop_10
    , bd_loop_100
    , bd_loop_1000
    , bd_map5
    , bd_message
    , bd_oneOf_first
    , bd_oneOf_last
    , bd_packet
    , bd_repeat_10
    , bd_repeat_100
    , bd_repeat_1000
    , bd_tagged50
    , br_andThen_5
    , br_loop_10
    , br_loop_100
    , br_loop_1000
    , br_map5
    , br_message
    , br_oneOf_first
    , br_oneOf_last
    , br_packet
    , br_tagged50
    , p2_andThen_5
    , p2_loop_10
    , p2_loop_100
    , p2_loop_1000
    , p2_map5
    , p2_message
    , p2_oneOf_first
    , p2_oneOf_last
    , p2_packet
    , p2_tagged50
    , raw_andThen_5
    , raw_loop_10
    , raw_loop_100
    , raw_loop_1000
    , raw_map5
    , raw_message
    , raw_packet
    , raw_tagged50
    , zw_andThen_5
    , zw_loop_10
    , zw_loop_100
    , zw_loop_1000
    , zw_map5
    , zw_message
    , zw_oneOf_first
    , zw_oneOf_last
    , zw_packet
    , zw_tagged50
    )

{-| Benchmarks comparing five Elm bytes decoding approaches.

Prefixes:

  - `raw_` — elm/bytes (baseline)
  - `bd_` — elm-cardano/bytes-decoder (this package)
  - `zw_` — zwilias/elm-bytes-parser (original)
  - `p2_` — Bytes.Parser2 (optimized copy of zw)
  - `br_` — mpizenberg/elm-bytes-decoder (Branchable)


## Sequential (applicative) — map5, 5 fields

    elm - bench -f Bench.raw_map5 -f Bench.bd_map5 -f Bench.zw_map5 -f Bench.p2_map5 -f Bench.br_map5 "()"


## Loop — 100 float64s

    elm - bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 -f Bench.zw_loop_100 -f Bench.p2_loop_100 -f Bench.br_loop_100 "()"


## Loop — 1000 float64s

    elm - bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 -f Bench.zw_loop_1000 -f Bench.p2_loop_1000 -f Bench.br_loop_1000 "()"


## andThen — 5 fields via chained andThen

    elm - bench -f Bench.raw_andThen_5 -f Bench.bd_andThen_5 -f Bench.zw_andThen_5 -f Bench.p2_andThen_5 -f Bench.br_andThen_5 "()"


## oneOf — first and last alternative

    elm - bench -f Bench.bd_oneOf_first -f Bench.zw_oneOf_first -f Bench.p2_oneOf_first -f Bench.br_oneOf_first "()"

    elm - bench -f Bench.bd_oneOf_last -f Bench.zw_oneOf_last -f Bench.p2_oneOf_last -f Bench.br_oneOf_last "()"


## Realistic 1 — all-fast packet (48 B, pure applicative)

    elm - bench -f Bench.raw_packet -f Bench.bd_packet -f Bench.zw_packet -f Bench.p2_packet -f Bench.br_packet "()"


## Realistic 2 — dynamic message (57 B, andThen + loop)

    elm - bench -f Bench.raw_message -f Bench.bd_message -f Bench.zw_message -f Bench.p2_message -f Bench.br_message "()"


## Realistic 3 — 50 tagged records with oneOf (175 B)

    elm - bench -f Bench.raw_tagged50 -f Bench.bd_tagged50 -f Bench.zw_tagged50 -f Bench.p2_tagged50 -f Bench.br_tagged50 "()"

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Decode.Branchable as BR
import Bytes.Decoder as BD
import Bytes.Encode as E
import Bytes.Parser as ZW
import Bytes.Parser2 as P2



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


{-| oneOf test data: tag 0 followed by 1 byte (matches first alternative).
-}
oneOfFirstData : Bytes
oneOfFirstData =
    E.encode
        (E.sequence
            [ E.unsignedInt8 0
            , E.unsignedInt8 42
            ]
        )


{-| oneOf test data: tag 2 followed by 4 bytes (matches third/last alternative).
-}
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


{-| Realistic 1: binary packet header + payload (48 bytes).
Exercises all primitive types + applicative combinators (keep, skip, ignore, repeat).
-}
packetData : Bytes
packetData =
    E.encode
        (E.sequence
            [ E.unsignedInt32 BE 0xDEADBEEF -- magic
            , E.unsignedInt16 BE 1 -- version
            , E.signedInt8 -1 -- flags
            , E.unsignedInt8 0 -- padding (skip)
            , E.float32 BE 1.5 -- temperature
            , E.float64 BE 3.14159 -- timestamp
            , E.unsignedInt8 0xCA -- payload byte 1
            , E.unsignedInt8 0xFE -- payload byte 2
            , E.unsignedInt8 0xBA -- payload byte 3
            , E.unsignedInt8 0xBE -- payload byte 4
            , E.string "Elm" -- label (3 bytes)
            , E.unsignedInt8 0xFF -- checksum (ignore)
            , E.sequence (List.map (E.unsignedInt16 BE) (List.range 1 10)) -- 10 u16 samples
            ]
        )


{-| Realistic 2: length-prefixed message (57 bytes).
Header (map4) + andThen on fieldCount + loop of (s32,f64) pairs + trailer (map4).
-}
messageData : Bytes
messageData =
    E.encode
        (E.sequence
            [ E.unsignedInt32 BE 0xCAFEBABE -- magic
            , E.unsignedInt16 BE 1 -- version
            , E.signedInt8 -5 -- priority
            , E.unsignedInt8 3 -- fieldCount
            , E.signedInt32 BE 100 -- field 1 key
            , E.float64 BE 1.1 -- field 1 value
            , E.signedInt32 BE 200 -- field 2 key
            , E.float64 BE 2.2 -- field 2 value
            , E.signedInt32 BE 300 -- field 3 key
            , E.float64 BE 3.3 -- field 3 value
            , E.float32 BE 99.9 -- checksum
            , E.signedInt16 BE -42 -- code
            , E.unsignedInt8 0xDE -- tag byte 1
            , E.unsignedInt8 0xAD -- tag byte 2
            , E.unsignedInt8 0xBE -- tag byte 3
            , E.unsignedInt8 0xEF -- tag byte 4
            , E.string "End" -- label (3 bytes)
            ]
        )


{-| Realistic 3: 50 tagged records alternating tag 0 and tag 2 (175 bytes).
25 tag-0 records (2 B each) + 25 tag-2 records (5 B each).
-}
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


raw_map5 : () -> Maybe Record5
raw_map5 () =
    D.decode (D.map5 Record5 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8) data5


raw_andThen_5 : () -> Maybe Record5
raw_andThen_5 () =
    D.decode
        (D.unsignedInt8
            |> D.andThen
                (\a ->
                    D.unsignedInt8
                        |> D.andThen
                            (\b ->
                                D.unsignedInt8
                                    |> D.andThen
                                        (\c ->
                                            D.unsignedInt8
                                                |> D.andThen
                                                    (\d ->
                                                        D.map (\e -> Record5 a b c d e) D.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5


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



-- Realistic benchmarks


raw_packet : () -> Maybe Packet
raw_packet () =
    D.decode rawPacketDecoder packetData


rawPacketDecoder : D.Decoder Packet
rawPacketDecoder =
    D.map5
        (\magic version flags _ temperature ->
            Packet magic version flags temperature
        )
        (D.unsignedInt32 BE)
        (D.unsignedInt16 BE)
        D.signedInt8
        (D.bytes 1)
        (D.float32 BE)
        |> D.andThen
            (\partial ->
                D.map5
                    (\timestamp payload label _ samples ->
                        partial timestamp payload label samples
                    )
                    (D.float64 BE)
                    (D.bytes 4)
                    (D.string 3)
                    (D.bytes 1)
                    (D.loop ( 10, [] )
                        (\( remaining, acc ) ->
                            if remaining <= 0 then
                                D.succeed (D.Done (List.reverse acc))

                            else
                                D.map (\v -> D.Loop ( remaining - 1, v :: acc )) (D.unsignedInt16 BE)
                        )
                    )
            )


raw_message : () -> Maybe Message
raw_message () =
    D.decode rawMessageDecoder messageData


rawMessageDecoder : D.Decoder Message
rawMessageDecoder =
    D.map3 (\magic version priority -> ( magic, version, priority ))
        (D.unsignedInt32 BE)
        (D.unsignedInt16 BE)
        D.signedInt8
        |> D.andThen
            (\( magic, version, priority ) ->
                D.unsignedInt8
                    |> D.andThen
                        (\fieldCount ->
                            D.loop ( fieldCount, [] )
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        D.succeed (D.Done (List.reverse acc))

                                    else
                                        D.map2 (\k v -> D.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (D.signedInt32 BE)
                                            (D.float64 BE)
                                )
                                |> D.andThen
                                    (\fields ->
                                        D.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (D.float32 BE)
                                            (D.signedInt16 BE)
                                            (D.bytes 4)
                                            (D.string 3)
                                    )
                        )
            )


raw_tagged50 : () -> Maybe (List OneOfResult)
raw_tagged50 () =
    D.decode
        (D.loop ( 50, [] )
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    D.succeed (D.Done (List.reverse acc))

                else
                    D.map (\v -> D.Loop ( remaining - 1, v :: acc )) rawTaggedDecoder
            )
        )
        tagged50Data


rawTaggedDecoder : D.Decoder OneOfResult
rawTaggedDecoder =
    D.unsignedInt8
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map Tag0 D.unsignedInt8

                    1 ->
                        D.map2 Tag1 D.unsignedInt8 D.unsignedInt8

                    2 ->
                        D.map4 Tag2 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8

                    _ ->
                        D.fail
            )



-- ============================================================================
-- elm-cardano/bytes-decoder (BD) — this package
-- ============================================================================


bd_map5 : () -> Maybe Record5
bd_map5 () =
    BD.decode
        (BD.map5 Record5 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
        data5
        |> Result.toMaybe


bd_keep5 : () -> Maybe Record5
bd_keep5 () =
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


bdLoopFloat64 : Int -> BD.Decoder c e (List Float)
bdLoopFloat64 n =
    BD.loop
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) (BD.float64 BE)
        )
        ( n, [] )


bd_repeat_10 : () -> Maybe (List Float)
bd_repeat_10 () =
    BD.decode (BD.repeat (BD.float64 BE) 10) floats10 |> Result.toMaybe


bd_repeat_100 : () -> Maybe (List Float)
bd_repeat_100 () =
    BD.decode (BD.repeat (BD.float64 BE) 100) floats100 |> Result.toMaybe


bd_repeat_1000 : () -> Maybe (List Float)
bd_repeat_1000 () =
    BD.decode (BD.repeat (BD.float64 BE) 1000) floats1000 |> Result.toMaybe


bd_loop_10 : () -> Maybe (List Float)
bd_loop_10 () =
    BD.decode (bdLoopFloat64 10) floats10 |> Result.toMaybe


bd_loop_100 : () -> Maybe (List Float)
bd_loop_100 () =
    BD.decode (bdLoopFloat64 100) floats100 |> Result.toMaybe


bd_loop_1000 : () -> Maybe (List Float)
bd_loop_1000 () =
    BD.decode (bdLoopFloat64 1000) floats1000 |> Result.toMaybe


bd_andThen_5 : () -> Maybe Record5
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
                                                        BD.map (\e -> Record5 a b c d e) BD.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5
        |> Result.toMaybe


type OneOfResult
    = Tag0 Int
    | Tag1 Int Int
    | Tag2 Int Int Int Int


bdOneOfDecoder : BD.Decoder c String OneOfResult
bdOneOfDecoder =
    BD.oneOf
        [ BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 0 then
                        BD.map Tag0 BD.unsignedInt8

                    else
                        BD.fail "not 0"
                )
        , BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 1 then
                        BD.map2 Tag1 BD.unsignedInt8 BD.unsignedInt8

                    else
                        BD.fail "not 1"
                )
        , BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 2 then
                        BD.map4 Tag2 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8

                    else
                        BD.fail "not 2"
                )
        ]


bd_oneOf_first : () -> Maybe OneOfResult
bd_oneOf_first () =
    BD.decode bdOneOfDecoder oneOfFirstData |> Result.toMaybe


bd_oneOf_last : () -> Maybe OneOfResult
bd_oneOf_last () =
    BD.decode bdOneOfDecoder oneOfLastData |> Result.toMaybe



-- Realistic benchmarks


bd_packet : () -> Maybe Packet
bd_packet () =
    BD.decode bdPacketDecoder packetData |> Result.toMaybe


bdPacketDecoder : BD.Decoder c e Packet
bdPacketDecoder =
    BD.succeed Packet
        |> BD.keep (BD.unsignedInt32 BE)
        |> BD.keep (BD.unsignedInt16 BE)
        |> BD.keep BD.signedInt8
        |> BD.skip 1
        |> BD.keep (BD.float32 BE)
        |> BD.keep (BD.float64 BE)
        |> BD.keep (BD.bytes 4)
        |> BD.keep (BD.string 3)
        |> BD.ignore BD.unsignedInt8
        |> BD.keep (BD.repeat (BD.unsignedInt16 BE) 10)


bd_message : () -> Maybe Message
bd_message () =
    BD.decode bdMessageDecoder messageData |> Result.toMaybe


bdMessageDecoder : BD.Decoder c e Message
bdMessageDecoder =
    BD.map3 (\magic version priority -> ( magic, version, priority ))
        (BD.unsignedInt32 BE)
        (BD.unsignedInt16 BE)
        BD.signedInt8
        |> BD.andThen
            (\( magic, version, priority ) ->
                BD.unsignedInt8
                    |> BD.andThen
                        (\fieldCount ->
                            BD.loop
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        BD.succeed (BD.Done (List.reverse acc))

                                    else
                                        BD.map2 (\k v -> BD.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (BD.signedInt32 BE)
                                            (BD.float64 BE)
                                )
                                ( fieldCount, [] )
                                |> BD.andThen
                                    (\fields ->
                                        BD.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (BD.float32 BE)
                                            (BD.signedInt16 BE)
                                            (BD.bytes 4)
                                            (BD.string 3)
                                    )
                        )
            )


bd_tagged50 : () -> Maybe (List OneOfResult)
bd_tagged50 () =
    BD.decode
        (BD.loop
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    BD.succeed (BD.Done (List.reverse acc))

                else
                    BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) bdOneOfDecoder
            )
            ( 50, [] )
        )
        tagged50Data
        |> Result.toMaybe



-- ============================================================================
-- zwilias/elm-bytes-parser (ZW)
-- ============================================================================


zw_map5 : () -> Maybe Record5
zw_map5 () =
    ZW.run
        (ZW.map5 Record5 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8)
        data5
        |> Result.toMaybe


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
        [ ZW.unsignedInt8
            |> ZW.andThen
                (\t ->
                    if t == 0 then
                        ZW.map Tag0 ZW.unsignedInt8

                    else
                        ZW.fail "not 0"
                )
        , ZW.unsignedInt8
            |> ZW.andThen
                (\t ->
                    if t == 1 then
                        ZW.map2 Tag1 ZW.unsignedInt8 ZW.unsignedInt8

                    else
                        ZW.fail "not 1"
                )
        , ZW.unsignedInt8
            |> ZW.andThen
                (\t ->
                    if t == 2 then
                        ZW.map4 Tag2 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8

                    else
                        ZW.fail "not 2"
                )
        ]


zw_oneOf_first : () -> Maybe OneOfResult
zw_oneOf_first () =
    ZW.run zwOneOfDecoder oneOfFirstData |> Result.toMaybe


zw_oneOf_last : () -> Maybe OneOfResult
zw_oneOf_last () =
    ZW.run zwOneOfDecoder oneOfLastData |> Result.toMaybe



-- Realistic benchmarks


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
-- Bytes.Parser2 (P2) — optimized copy of ZW
-- ============================================================================


p2_map5 : () -> Maybe Record5
p2_map5 () =
    P2.run
        (P2.map5 Record5 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8)
        data5
        |> Result.toMaybe


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
        [ P2.unsignedInt8
            |> P2.andThen
                (\t ->
                    if t == 0 then
                        P2.map Tag0 P2.unsignedInt8

                    else
                        P2.fail "not 0"
                )
        , P2.unsignedInt8
            |> P2.andThen
                (\t ->
                    if t == 1 then
                        P2.map2 Tag1 P2.unsignedInt8 P2.unsignedInt8

                    else
                        P2.fail "not 1"
                )
        , P2.unsignedInt8
            |> P2.andThen
                (\t ->
                    if t == 2 then
                        P2.map4 Tag2 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8 P2.unsignedInt8

                    else
                        P2.fail "not 2"
                )
        ]


p2_oneOf_first : () -> Maybe OneOfResult
p2_oneOf_first () =
    P2.run p2OneOfDecoder oneOfFirstData |> Result.toMaybe


p2_oneOf_last : () -> Maybe OneOfResult
p2_oneOf_last () =
    P2.run p2OneOfDecoder oneOfLastData |> Result.toMaybe



-- Realistic benchmarks


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
-- mpizenberg/elm-bytes-decoder Branchable (BR)
-- ============================================================================


br_map5 : () -> Maybe Record5
br_map5 () =
    BR.decode
        (BR.map5 Record5 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8)
        data5


brLoopFloat64 : Int -> BR.Decoder (List Float)
brLoopFloat64 n =
    BR.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BR.succeed (D.Done (List.reverse acc))

            else
                BR.map (\v -> D.Loop ( remaining - 1, v :: acc )) (BR.float64 BE)
        )


br_loop_10 : () -> Maybe (List Float)
br_loop_10 () =
    BR.decode (brLoopFloat64 10) floats10


br_loop_100 : () -> Maybe (List Float)
br_loop_100 () =
    BR.decode (brLoopFloat64 100) floats100


br_loop_1000 : () -> Maybe (List Float)
br_loop_1000 () =
    BR.decode (brLoopFloat64 1000) floats1000


br_andThen_5 : () -> Maybe Record5
br_andThen_5 () =
    BR.decode
        (BR.unsignedInt8
            |> BR.andThen
                (\a ->
                    BR.unsignedInt8
                        |> BR.andThen
                            (\b ->
                                BR.unsignedInt8
                                    |> BR.andThen
                                        (\c ->
                                            BR.unsignedInt8
                                                |> BR.andThen
                                                    (\d ->
                                                        BR.map (\e -> Record5 a b c d e) BR.unsignedInt8
                                                    )
                                        )
                            )
                )
        )
        data5


brOneOfDecoder : BR.Decoder OneOfResult
brOneOfDecoder =
    BR.oneOf
        [ BR.unsignedInt8
            |> BR.andThen
                (\t ->
                    if t == 0 then
                        BR.map Tag0 BR.unsignedInt8

                    else
                        BR.fail
                )
        , BR.unsignedInt8
            |> BR.andThen
                (\t ->
                    if t == 1 then
                        BR.map2 Tag1 BR.unsignedInt8 BR.unsignedInt8

                    else
                        BR.fail
                )
        , BR.unsignedInt8
            |> BR.andThen
                (\t ->
                    if t == 2 then
                        BR.map4 Tag2 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8

                    else
                        BR.fail
                )
        ]


br_oneOf_first : () -> Maybe OneOfResult
br_oneOf_first () =
    BR.decode brOneOfDecoder oneOfFirstData


br_oneOf_last : () -> Maybe OneOfResult
br_oneOf_last () =
    BR.decode brOneOfDecoder oneOfLastData



-- Realistic benchmarks


br_packet : () -> Maybe Packet
br_packet () =
    BR.decode brPacketDecoder packetData


brPacketDecoder : BR.Decoder Packet
brPacketDecoder =
    BR.succeed Packet
        |> BR.keep (BR.unsignedInt32 BE)
        |> BR.keep (BR.unsignedInt16 BE)
        |> BR.keep BR.signedInt8
        |> BR.skip 1
        |> BR.keep (BR.float32 BE)
        |> BR.keep (BR.float64 BE)
        |> BR.keep (BR.bytes 4)
        |> BR.keep (BR.string 3)
        |> BR.ignore BR.unsignedInt8
        |> BR.keep (BR.repeat (BR.unsignedInt16 BE) 10)


br_message : () -> Maybe Message
br_message () =
    BR.decode brMessageDecoder messageData


brMessageDecoder : BR.Decoder Message
brMessageDecoder =
    BR.map3 (\magic version priority -> ( magic, version, priority ))
        (BR.unsignedInt32 BE)
        (BR.unsignedInt16 BE)
        BR.signedInt8
        |> BR.andThen
            (\( magic, version, priority ) ->
                BR.unsignedInt8
                    |> BR.andThen
                        (\fieldCount ->
                            BR.loop ( fieldCount, [] )
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        BR.succeed (D.Done (List.reverse acc))

                                    else
                                        BR.map2 (\k v -> D.Loop ( remaining - 1, ( k, v ) :: acc ))
                                            (BR.signedInt32 BE)
                                            (BR.float64 BE)
                                )
                                |> BR.andThen
                                    (\fields ->
                                        BR.map4
                                            (\checksum code tag label ->
                                                Message magic version priority fields checksum code tag label
                                            )
                                            (BR.float32 BE)
                                            (BR.signedInt16 BE)
                                            (BR.bytes 4)
                                            (BR.string 3)
                                    )
                        )
            )


br_tagged50 : () -> Maybe (List OneOfResult)
br_tagged50 () =
    BR.decode
        (BR.loop ( 50, [] )
            (\( remaining, acc ) ->
                if remaining <= 0 then
                    BR.succeed (D.Done (List.reverse acc))

                else
                    BR.map (\v -> D.Loop ( remaining - 1, v :: acc )) brOneOfDecoder
            )
        )
        tagged50Data
