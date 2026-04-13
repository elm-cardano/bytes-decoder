module Bench exposing
    ( raw_map5
    , raw_andThen_5
    , raw_loop_10
    , raw_loop_100
    , raw_loop_1000
    , bd_map5
    , bd_keep5
    , bd_repeat_10
    , bd_repeat_100
    , bd_repeat_1000
    , bd_loop_10
    , bd_loop_100
    , bd_loop_1000
    , bd_andThen_5
    , bd_oneOf_first
    , bd_oneOf_last
    , zw_map5
    , zw_loop_10
    , zw_loop_100
    , zw_loop_1000
    , zw_andThen_5
    , zw_oneOf_first
    , zw_oneOf_last
    , br_map5
    , br_loop_10
    , br_loop_100
    , br_loop_1000
    , br_andThen_5
    , br_oneOf_first
    , br_oneOf_last
    )

{-| Benchmarks comparing four Elm bytes decoding approaches.

Prefixes:

  - `raw_` — elm/bytes (baseline)
  - `bd_` — elm-cardano/bytes-decoder (this package)
  - `zw_` — zwilias/elm-bytes-parser
  - `br_` — mpizenberg/elm-bytes-decoder (Branchable)


## Sequential (applicative) — map5, 5 fields

    elm-bench -f Bench.raw_map5 -f Bench.bd_map5 -f Bench.zw_map5 -f Bench.br_map5 "()"


## Loop — 100 float64s

    elm-bench -f Bench.raw_loop_100 -f Bench.bd_repeat_100 -f Bench.bd_loop_100 -f Bench.zw_loop_100 -f Bench.br_loop_100 "()"


## Loop — 1000 float64s

    elm-bench -f Bench.raw_loop_1000 -f Bench.bd_repeat_1000 -f Bench.bd_loop_1000 -f Bench.zw_loop_1000 -f Bench.br_loop_1000 "()"


## andThen — 5 fields via chained andThen

    elm-bench -f Bench.raw_andThen_5 -f Bench.bd_andThen_5 -f Bench.zw_andThen_5 -f Bench.br_andThen_5 "()"


## oneOf — first and last alternative

    elm-bench -f Bench.bd_oneOf_first -f Bench.zw_oneOf_first -f Bench.br_oneOf_first "()"
    elm-bench -f Bench.bd_oneOf_last -f Bench.zw_oneOf_last -f Bench.br_oneOf_last "()"

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Decode.Branchable as BR
import Bytes.Decoder as BD
import Bytes.Encode as E
import Bytes.Parser as ZW



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



-- ============================================================================
-- RAW elm/bytes
-- ============================================================================


type alias Record5 =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


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


bdLoopFloat64 : Int -> BD.Decoder e (List Float)
bdLoopFloat64 n =
    BD.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) (BD.float64 BE)
        )


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


bdOneOfDecoder : BD.Decoder String OneOfResult
bdOneOfDecoder =
    BD.oneOf
        [ BD.unsignedInt8 |> BD.andThen (\t -> if t == 0 then BD.map Tag0 BD.unsignedInt8 else BD.fail "not 0")
        , BD.unsignedInt8 |> BD.andThen (\t -> if t == 1 then BD.map2 Tag1 BD.unsignedInt8 BD.unsignedInt8 else BD.fail "not 1")
        , BD.unsignedInt8 |> BD.andThen (\t -> if t == 2 then BD.map4 Tag2 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 else BD.fail "not 2")
        ]


bd_oneOf_first : () -> Maybe OneOfResult
bd_oneOf_first () =
    BD.decode bdOneOfDecoder oneOfFirstData |> Result.toMaybe


bd_oneOf_last : () -> Maybe OneOfResult
bd_oneOf_last () =
    BD.decode bdOneOfDecoder oneOfLastData |> Result.toMaybe



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
        [ BR.unsignedInt8 |> BR.andThen (\t -> if t == 0 then BR.map Tag0 BR.unsignedInt8 else BR.fail)
        , BR.unsignedInt8 |> BR.andThen (\t -> if t == 1 then BR.map2 Tag1 BR.unsignedInt8 BR.unsignedInt8 else BR.fail)
        , BR.unsignedInt8 |> BR.andThen (\t -> if t == 2 then BR.map4 Tag2 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 else BR.fail)
        ]


br_oneOf_first : () -> Maybe OneOfResult
br_oneOf_first () =
    BR.decode brOneOfDecoder oneOfFirstData


br_oneOf_last : () -> Maybe OneOfResult
br_oneOf_last () =
    BR.decode brOneOfDecoder oneOfLastData
