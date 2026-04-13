module Tests exposing (suite)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decoder as BD exposing (Error(..))
import Bytes.Encode as E
import Expect
import Test exposing (Test, describe, test)



-- HELPERS


{-| Build a Bytes value from a list of unsigned ints (0–255).
-}
fromList : List Int -> Bytes
fromList ints =
    E.encode (E.sequence (List.map E.unsignedInt8 ints))


{-| Encode n big-endian float64 values.
-}
floatBytes : List Float -> Bytes
floatBytes fs =
    E.encode (E.sequence (List.map (E.float64 BE) fs))


{-| Empty bytes.
-}
empty : Bytes
empty =
    E.encode (E.sequence [])



-- TEST SUITE


suite : Test
suite =
    describe "Bytes.Decoder"
        [ primitiveTests
        , staticTests
        , mappingTests
        , chainingTests
        , pipelineTests
        , branchingTests
        , loopingTests
        , randomAccessTests
        , errorTests
        , edgeCaseTests
        ]



-- PRIMITIVES


primitiveTests : Test
primitiveTests =
    describe "Primitives"
        [ describe "unsignedInt8"
            [ test "decodes 0" <|
                \_ ->
                    BD.decode BD.unsignedInt8 (fromList [ 0 ])
                        |> Expect.equal (Ok 0)
            , test "decodes 255" <|
                \_ ->
                    BD.decode BD.unsignedInt8 (fromList [ 255 ])
                        |> Expect.equal (Ok 255)
            , test "decodes 42" <|
                \_ ->
                    BD.decode BD.unsignedInt8 (fromList [ 42 ])
                        |> Expect.equal (Ok 42)
            , test "fails on empty input" <|
                \_ ->
                    BD.decode BD.unsignedInt8 empty
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 1 }))
            ]
        , describe "signedInt8"
            [ test "decodes positive" <|
                \_ ->
                    BD.decode BD.signedInt8 (fromList [ 127 ])
                        |> Expect.equal (Ok 127)
            , test "decodes negative" <|
                \_ ->
                    BD.decode BD.signedInt8 (fromList [ 0xFF ])
                        |> Expect.equal (Ok -1)
            , test "decodes -128" <|
                \_ ->
                    BD.decode BD.signedInt8 (fromList [ 0x80 ])
                        |> Expect.equal (Ok -128)
            ]
        , describe "unsignedInt16"
            [ test "decodes BE" <|
                \_ ->
                    BD.decode (BD.unsignedInt16 BE) (fromList [ 0x01, 0x00 ])
                        |> Expect.equal (Ok 256)
            , test "decodes LE" <|
                \_ ->
                    BD.decode (BD.unsignedInt16 LE) (fromList [ 0x00, 0x01 ])
                        |> Expect.equal (Ok 256)
            , test "fails on 1 byte" <|
                \_ ->
                    BD.decode (BD.unsignedInt16 BE) (fromList [ 0x01 ])
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 2 }))
            ]
        , describe "signedInt16"
            [ test "decodes negative BE" <|
                \_ ->
                    BD.decode (BD.signedInt16 BE) (fromList [ 0xFF, 0xFE ])
                        |> Expect.equal (Ok -2)
            ]
        , describe "unsignedInt32"
            [ test "decodes BE" <|
                \_ ->
                    BD.decode (BD.unsignedInt32 BE) (fromList [ 0, 0, 1, 0 ])
                        |> Expect.equal (Ok 256)
            , test "fails on 3 bytes" <|
                \_ ->
                    BD.decode (BD.unsignedInt32 BE) (fromList [ 0, 0, 1 ])
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 4 }))
            ]
        , describe "signedInt32"
            [ test "decodes negative BE" <|
                \_ ->
                    BD.decode (BD.signedInt32 BE) (fromList [ 0xFF, 0xFF, 0xFF, 0xFF ])
                        |> Expect.equal (Ok -1)
            ]
        , describe "float64"
            [ test "round-trips pi" <|
                \_ ->
                    BD.decode (BD.float64 BE) (floatBytes [ pi ])
                        |> Expect.equal (Ok pi)
            ]
        , describe "float32"
            [ test "decodes a float32" <|
                \_ ->
                    let
                        input : Bytes
                        input =
                            E.encode (E.float32 BE 1.5)
                    in
                    BD.decode (BD.float32 BE) input
                        |> Expect.equal (Ok 1.5)
            ]
        , describe "bytes"
            [ test "decodes N bytes" <|
                \_ ->
                    BD.decode (BD.bytes 3) (fromList [ 10, 20, 30 ])
                        |> Result.map Bytes.width
                        |> Expect.equal (Ok 3)
            , test "decodes 0 bytes from empty" <|
                \_ ->
                    BD.decode (BD.bytes 0) empty
                        |> Result.map Bytes.width
                        |> Expect.equal (Ok 0)
            , test "fails when not enough bytes" <|
                \_ ->
                    BD.decode (BD.bytes 5) (fromList [ 1, 2, 3 ])
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 5 }))
            ]
        , describe "string"
            [ test "decodes ASCII" <|
                \_ ->
                    let
                        input : Bytes
                        input =
                            E.encode (E.string "hello")
                    in
                    BD.decode (BD.string 5) input
                        |> Expect.equal (Ok "hello")
            , test "decodes UTF-8" <|
                \_ ->
                    let
                        s : String
                        s =
                            "café"

                        input : Bytes
                        input =
                            E.encode (E.string s)
                    in
                    BD.decode (BD.string (Bytes.width input)) input
                        |> Expect.equal (Ok s)
            , test "fails when too short" <|
                \_ ->
                    BD.decode (BD.string 10) (fromList [ 0x41 ])
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 10 }))
            ]
        ]



-- STATIC


staticTests : Test
staticTests =
    describe "Static"
        [ test "succeed returns value without consuming bytes" <|
            \_ ->
                BD.decode (BD.succeed 42) empty
                    |> Expect.equal (Ok 42)
        , test "succeed works with non-empty input" <|
            \_ ->
                BD.decode (BD.succeed "hello") (fromList [ 1, 2, 3 ])
                    |> Expect.equal (Ok "hello")
        , test "fail returns Custom error at offset 0" <|
            \_ ->
                BD.decode (BD.fail "oops") empty
                    |> Expect.equal (Err (Custom { at = 0 } "oops"))
        , test "fail returns Custom error even with input" <|
            \_ ->
                BD.decode (BD.fail "oops") (fromList [ 1, 2, 3 ])
                    |> Expect.equal (Err (Custom { at = 0 } "oops"))
        ]



-- MAPPING


mappingTests : Test
mappingTests =
    describe "Mapping"
        [ describe "map"
            [ test "transforms value" <|
                \_ ->
                    BD.decode (BD.map (\n -> n * 2) BD.unsignedInt8) (fromList [ 5 ])
                        |> Expect.equal (Ok 10)
            , test "propagates error" <|
                \_ ->
                    BD.decode (BD.map (\n -> n * 2) BD.unsignedInt8) empty
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 1 }))
            ]
        , describe "map2"
            [ test "combines two values" <|
                \_ ->
                    BD.decode (BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8) (fromList [ 1, 2 ])
                        |> Expect.equal (Ok ( 1, 2 ))
            , test "fails on first decoder" <|
                \_ ->
                    BD.decode (BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8) empty
                        |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 1 }))
            , test "fails on second decoder" <|
                \_ ->
                    BD.decode (BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8) (fromList [ 1 ])
                        |> Expect.equal (Err (OutOfBounds { at = 1, bytes = 1 }))
            , test "works with mixed types" <|
                \_ ->
                    let
                        input : Bytes
                        input =
                            E.encode
                                (E.sequence
                                    [ E.unsignedInt8 0xFF
                                    , E.float64 BE 2.5
                                    ]
                                )
                    in
                    BD.decode (BD.map2 Tuple.pair BD.unsignedInt8 (BD.float64 BE)) input
                        |> Expect.equal (Ok ( 255, 2.5 ))
            ]
        , describe "map3"
            [ test "combines three values" <|
                \_ ->
                    BD.decode (BD.map3 (\a b c -> ( a, b, c )) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8) (fromList [ 10, 20, 30 ])
                        |> Expect.equal (Ok ( 10, 20, 30 ))
            , test "fails on third decoder" <|
                \_ ->
                    BD.decode (BD.map3 (\a b c -> ( a, b, c )) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8) (fromList [ 10, 20 ])
                        |> Expect.equal (Err (OutOfBounds { at = 2, bytes = 1 }))
            ]
        , describe "map4"
            [ test "combines four values" <|
                \_ ->
                    BD.decode
                        (BD.map4 (\a b c d -> [ a, b, c, d ]) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
                        (fromList [ 1, 2, 3, 4 ])
                        |> Expect.equal (Ok [ 1, 2, 3, 4 ])
            ]
        , describe "map5"
            [ test "combines five values" <|
                \_ ->
                    BD.decode
                        (BD.map5 (\a b c d e -> [ a, b, c, d, e ]) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
                        (fromList [ 1, 2, 3, 4, 5 ])
                        |> Expect.equal (Ok [ 1, 2, 3, 4, 5 ])
            , test "fails when input too short" <|
                \_ ->
                    BD.decode
                        (BD.map5 (\a b c d e -> [ a, b, c, d, e ]) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
                        (fromList [ 1, 2, 3 ])
                        |> Expect.equal (Err (OutOfBounds { at = 3, bytes = 1 }))
            ]
        ]



-- CHAINING


chainingTests : Test
chainingTests =
    describe "andThen"
        [ test "chains decoders based on value" <|
            \_ ->
                let
                    decoder : BD.Decoder context error String
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen
                                (\len ->
                                    BD.string len
                                )
                in
                BD.decode decoder
                    (E.encode
                        (E.sequence
                            [ E.unsignedInt8 5
                            , E.string "hello"
                            ]
                        )
                    )
                    |> Expect.equal (Ok "hello")
        , test "tag-based dispatching" <|
            \_ ->
                let
                    decoder : BD.Decoder context String ( String, Int )
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen
                                (\tag ->
                                    case tag of
                                        1 ->
                                            BD.map (\v -> ( "u8", v )) BD.unsignedInt8

                                        2 ->
                                            BD.map (\v -> ( "u16", v )) (BD.unsignedInt16 BE)

                                        _ ->
                                            BD.fail "unknown tag"
                                )
                in
                BD.decode decoder (fromList [ 2, 0x01, 0x00 ])
                    |> Expect.equal (Ok ( "u16", 256 ))
        , test "fail in andThen reports correct offset" <|
            \_ ->
                let
                    decoder : BD.Decoder context String value
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen (\_ -> BD.fail "nope")
                in
                BD.decode decoder (fromList [ 42 ])
                    |> Expect.equal (Err (Custom { at = 1 } "nope"))
        , test "propagates first decoder error" <|
            \_ ->
                let
                    decoder : BD.Decoder context error String
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen (\_ -> BD.succeed "ok")
                in
                BD.decode decoder empty
                    |> Expect.equal (Err (OutOfBounds { at = 0, bytes = 1 }))
        , test "chained andThen with multiple reads" <|
            \_ ->
                let
                    decoder : BD.Decoder context error ( Int, Int )
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen
                                (\a ->
                                    BD.unsignedInt8
                                        |> BD.andThen
                                            (\b ->
                                                BD.succeed ( a, b )
                                            )
                                )
                in
                BD.decode decoder (fromList [ 10, 20 ])
                    |> Expect.equal (Ok ( 10, 20 ))
        ]



-- PIPELINE


pipelineTests : Test
pipelineTests =
    describe "Pipeline"
        [ describe "keep"
            [ test "builds a record" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error { x : Int, y : Int, z : Int }
                        decoder =
                            BD.succeed (\a b c -> { x = a, y = b, z = c })
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 2, 3 ])
                        |> Expect.equal (Ok { x = 1, y = 2, z = 3 })
            , test "fails with correct offset in pipeline" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error ( Int, Int, Int )
                        decoder =
                            BD.succeed (\a b c -> ( a, b, c ))
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 2 ])
                        |> Expect.equal (Err (OutOfBounds { at = 2, bytes = 1 }))
            ]
        , describe "ignore"
            [ test "skips decoder result" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error Int
                        decoder =
                            BD.succeed identity
                                |> BD.keep BD.unsignedInt8
                                |> BD.ignore BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 42, 99 ])
                        |> Expect.equal (Ok 42)
            , test "advances offset past ignored bytes" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error ( Int, Int )
                        decoder =
                            BD.succeed Tuple.pair
                                |> BD.keep BD.unsignedInt8
                                |> BD.ignore BD.unsignedInt8
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 99, 3 ])
                        |> Expect.equal (Ok ( 1, 3 ))
            ]
        , describe "skip"
            [ test "skip in pipeline skips bytes between fields" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error ( Int, Int )
                        decoder =
                            BD.succeed Tuple.pair
                                |> BD.keep BD.unsignedInt8
                                |> BD.skip 2
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 0, 0, 42 ])
                        |> Expect.equal (Ok ( 1, 42 ))
            , test "skip in pipeline with multiple skips" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error ( Int, Int, Int )
                        decoder =
                            BD.succeed (\a b c -> ( a, b, c ))
                                |> BD.keep BD.unsignedInt8
                                |> BD.skip 1
                                |> BD.keep BD.unsignedInt8
                                |> BD.skip 1
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 99, 2, 99, 3 ])
                        |> Expect.equal (Ok ( 1, 2, 3 ))
            , test "fails if not enough bytes to skip" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error Int
                        decoder =
                            BD.succeed identity
                                |> BD.keep BD.unsignedInt8
                                |> BD.skip 10
                    in
                    BD.decode decoder (fromList [ 1 ])
                        |> Expect.equal (Err (OutOfBounds { at = 1, bytes = 10 }))
            ]
        ]



-- BRANCHING


branchingTests : Test
branchingTests =
    describe "oneOf"
        [ test "picks first matching decoder" <|
            \_ ->
                let
                    decoder : BD.Decoder context String ( String, Int )
                    decoder =
                        BD.oneOf
                            [ BD.map2 (\_ v -> ( "a", v ))
                                (BD.unsignedInt8
                                    |> BD.andThen
                                        (\t ->
                                            if t == 1 then
                                                BD.succeed t

                                            else
                                                BD.fail "not 1"
                                        )
                                )
                                BD.unsignedInt8
                            , BD.map2 (\_ v -> ( "b", v ))
                                (BD.unsignedInt8
                                    |> BD.andThen
                                        (\t ->
                                            if t == 2 then
                                                BD.succeed t

                                            else
                                                BD.fail "not 2"
                                        )
                                )
                                BD.unsignedInt8
                            ]
                in
                BD.decode decoder (fromList [ 1, 42 ])
                    |> Expect.equal (Ok ( "a", 42 ))
        , test "picks second alternative" <|
            \_ ->
                let
                    decoder : BD.Decoder context String String
                    decoder =
                        BD.oneOf
                            [ BD.unsignedInt8
                                |> BD.andThen
                                    (\t ->
                                        if t == 1 then
                                            BD.succeed "one"

                                        else
                                            BD.fail "not 1"
                                    )
                            , BD.unsignedInt8
                                |> BD.andThen
                                    (\t ->
                                        if t == 2 then
                                            BD.succeed "two"

                                        else
                                            BD.fail "not 2"
                                    )
                            ]
                in
                BD.decode decoder (fromList [ 2 ])
                    |> Expect.equal (Ok "two")
        , test "collects all errors on failure" <|
            \_ ->
                let
                    decoder : BD.Decoder context String value
                    decoder =
                        BD.oneOf
                            [ BD.fail "err1"
                            , BD.fail "err2"
                            , BD.fail "err3"
                            ]
                in
                BD.decode decoder empty
                    |> Expect.equal
                        (Err
                            (BadOneOf { at = 0 }
                                [ Custom { at = 0 } "err1"
                                , Custom { at = 0 } "err2"
                                , Custom { at = 0 } "err3"
                                ]
                            )
                        )
        , test "empty oneOf fails" <|
            \_ ->
                BD.decode (BD.oneOf []) empty
                    |> Expect.equal (Err (BadOneOf { at = 0 } []))
        , test "oneOf resets offset for each alternative" <|
            \_ ->
                let
                    -- First alternative reads 2 bytes then fails
                    -- Second alternative should still read from offset 0
                    decoder : BD.Decoder context String ( Int, Int )
                    decoder =
                        BD.oneOf
                            [ BD.unsignedInt8
                                |> BD.andThen
                                    (\_ ->
                                        BD.unsignedInt8
                                            |> BD.andThen (\_ -> BD.fail "nope")
                                    )
                            , BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8
                            ]
                in
                BD.decode decoder (fromList [ 0xAA, 0xBB ])
                    |> Expect.equal (Ok ( 0xAA, 0xBB ))
        , test "oneOf at non-zero offset" <|
            \_ ->
                let
                    decoder : BD.Decoder context String String
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen
                                (\_ ->
                                    BD.oneOf
                                        [ BD.unsignedInt8
                                            |> BD.andThen
                                                (\t ->
                                                    if t == 1 then
                                                        BD.succeed "one"

                                                    else
                                                        BD.fail "not 1"
                                                )
                                        , BD.unsignedInt8
                                            |> BD.andThen
                                                (\t ->
                                                    if t == 2 then
                                                        BD.succeed "two"

                                                    else
                                                        BD.fail "not 2"
                                                )
                                        ]
                                )
                in
                BD.decode decoder (fromList [ 0xFF, 2 ])
                    |> Expect.equal (Ok "two")
        , test "nested oneOf" <|
            \_ ->
                let
                    inner : BD.Decoder context String String
                    inner =
                        BD.oneOf
                            [ BD.unsignedInt8
                                |> BD.andThen
                                    (\t ->
                                        if t == 10 then
                                            BD.succeed "ten"

                                        else
                                            BD.fail "not 10"
                                    )
                            , BD.unsignedInt8
                                |> BD.andThen
                                    (\t ->
                                        if t == 20 then
                                            BD.succeed "twenty"

                                        else
                                            BD.fail "not 20"
                                    )
                            ]

                    decoder : BD.Decoder context String String
                    decoder =
                        BD.oneOf
                            [ BD.fail "skip"
                            , inner
                            ]
                in
                BD.decode decoder (fromList [ 20 ])
                    |> Expect.equal (Ok "twenty")
        ]



-- LOOPING


loopingTests : Test
loopingTests =
    describe "Looping"
        [ describe "loop"
            [ test "decodes counted list" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error (List Int)
                        decoder =
                            BD.unsignedInt8
                                |> BD.andThen
                                    (\count ->
                                        BD.loop
                                            (\( remaining, acc ) ->
                                                if remaining <= 0 then
                                                    BD.succeed (BD.Done (List.reverse acc))

                                                else
                                                    BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) BD.unsignedInt8
                                            )
                                            ( count, [] )
                                    )
                    in
                    BD.decode decoder (fromList [ 3, 10, 20, 30 ])
                        |> Expect.equal (Ok [ 10, 20, 30 ])
            , test "zero iterations" <|
                \_ ->
                    BD.decode
                        (BD.loop
                            (\n ->
                                if n <= 0 then
                                    BD.succeed (BD.Done "done")

                                else
                                    BD.map (\_ -> BD.Loop (n - 1)) BD.unsignedInt8
                            )
                            0
                        )
                        empty
                        |> Expect.equal (Ok "done")
            , test "fails mid-loop" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error (List Int)
                        decoder =
                            BD.loop
                                (\( remaining, acc ) ->
                                    if remaining <= 0 then
                                        BD.succeed (BD.Done (List.reverse acc))

                                    else
                                        BD.map (\v -> BD.Loop ( remaining - 1, v :: acc )) BD.unsignedInt8
                                )
                                ( 5, [] )
                    in
                    BD.decode decoder (fromList [ 1, 2 ])
                        |> Expect.equal (Err (OutOfBounds { at = 2, bytes = 1 }))
            ]
        , describe "repeat"
            [ test "decodes N items" <|
                \_ ->
                    BD.decode (BD.repeat BD.unsignedInt8 4) (fromList [ 10, 20, 30, 40 ])
                        |> Expect.equal (Ok [ 10, 20, 30, 40 ])
            , test "repeat 0 returns empty list" <|
                \_ ->
                    BD.decode (BD.repeat BD.unsignedInt8 0) empty
                        |> Expect.equal (Ok [])
            , test "repeat negative returns empty list" <|
                \_ ->
                    BD.decode (BD.repeat BD.unsignedInt8 -1) empty
                        |> Expect.equal (Ok [])
            , test "repeat fails when input too short" <|
                \_ ->
                    BD.decode (BD.repeat BD.unsignedInt8 5) (fromList [ 1, 2 ])
                        |> Expect.equal (Err (OutOfBounds { at = 2, bytes = 1 }))
            , test "repeat with compound decoder" <|
                \_ ->
                    let
                        pair : BD.Decoder context error ( Int, Int )
                        pair =
                            BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8
                    in
                    BD.decode (BD.repeat pair 2) (fromList [ 1, 2, 3, 4 ])
                        |> Expect.equal (Ok [ ( 1, 2 ), ( 3, 4 ) ])
            , test "repeat with float64" <|
                \_ ->
                    BD.decode (BD.repeat (BD.float64 BE) 3) (floatBytes [ 1.0, 2.0, 3.0 ])
                        |> Expect.equal (Ok [ 1.0, 2.0, 3.0 ])
            ]
        ]



-- RANDOM ACCESS


randomAccessTests : Test
randomAccessTests =
    describe "Random access"
        [ describe "position"
            [ test "returns startOfInput at offset 0" <|
                \_ ->
                    BD.decode BD.position (fromList [ 1, 2, 3 ])
                        |> Expect.equal (Ok BD.startOfInput)
            , test "returns offset after reading" <|
                \_ ->
                    BD.decode
                        (BD.unsignedInt8
                            |> BD.andThen (\_ -> BD.position)
                        )
                        (fromList [ 1, 2, 3 ])
                        |> Result.map (\p -> p /= BD.startOfInput)
                        |> Expect.equal (Ok True)
            , test "consumes no bytes" <|
                \_ ->
                    BD.decode
                        (BD.map2 Tuple.pair BD.position BD.unsignedInt8)
                        (fromList [ 42 ])
                        |> Result.map Tuple.second
                        |> Expect.equal (Ok 42)
            ]
        , describe "randomAccess"
            [ test "reads at absolute offset" <|
                \_ ->
                    let
                        -- [0, 0, 0, 42] — read the byte at offset 3
                        decoder : BD.Decoder context error Int
                        decoder =
                            BD.randomAccess
                                { offset = 3, relativeTo = BD.startOfInput }
                                BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 0, 0, 0, 42 ])
                        |> Expect.equal (Ok 42)
            , test "resumes at original position" <|
                \_ ->
                    let
                        -- Read u8 at offset 0, jump to offset 3 for another u8, then read u8 at offset 1
                        decoder : BD.Decoder context error ( Int, Int, Int )
                        decoder =
                            BD.succeed (\a b c -> ( a, b, c ))
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep
                                    (BD.randomAccess
                                        { offset = 3, relativeTo = BD.startOfInput }
                                        BD.unsignedInt8
                                    )
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 2, 0, 42 ])
                        |> Expect.equal (Ok ( 1, 42, 2 ))
            , test "with position-relative offset" <|
                \_ ->
                    let
                        -- Byte 0: length (5), Byte 1: relative offset (2), Byte 2: number (99),
                        -- Bytes 3-7: "hello"
                        input : Bytes
                        input =
                            E.encode
                                (E.sequence
                                    [ E.unsignedInt8 5
                                    , E.unsignedInt8 2
                                    , E.unsignedInt8 99
                                    , E.string "hello"
                                    ]
                                )

                        decoder : BD.Decoder context error String
                        decoder =
                            BD.succeed Tuple.pair
                                |> BD.keep BD.unsignedInt8
                                |> BD.keep BD.position
                                |> BD.andThen
                                    (\( len, marker ) ->
                                        BD.unsignedInt8
                                            |> BD.andThen
                                                (\relOffset ->
                                                    BD.randomAccess
                                                        { offset = relOffset, relativeTo = marker }
                                                        (BD.string len)
                                                )
                                    )
                    in
                    BD.decode decoder input
                        |> Expect.equal (Ok "hello")
            , test "resumes after randomAccess in pipeline" <|
                \_ ->
                    let
                        input : Bytes
                        input =
                            E.encode
                                (E.sequence
                                    [ E.unsignedInt8 5
                                    , E.unsignedInt8 15
                                    , E.unsignedInt8 6
                                    , E.string (String.repeat 12 "\u{0000}")
                                    , E.string "hello"
                                    ]
                                )

                        stringAtOffset : BD.Decoder context error String
                        stringAtOffset =
                            BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8
                                |> BD.andThen
                                    (\( len, offset ) ->
                                        BD.randomAccess
                                            { offset = offset, relativeTo = BD.startOfInput }
                                            (BD.string len)
                                    )

                        decoder : BD.Decoder context error { number : Int, string : String }
                        decoder =
                            BD.succeed (\s n -> { string = s, number = n })
                                |> BD.keep stringAtOffset
                                |> BD.keep BD.unsignedInt8
                    in
                    BD.decode decoder input
                        |> Expect.equal (Ok { string = "hello", number = 6 })
            , test "error in randomAccess propagates" <|
                \_ ->
                    let
                        decoder : BD.Decoder context error Int
                        decoder =
                            BD.randomAccess
                                { offset = 100, relativeTo = BD.startOfInput }
                                BD.unsignedInt8
                    in
                    BD.decode decoder (fromList [ 1, 2, 3 ])
                        |> Expect.equal (Err (OutOfBounds { at = 100, bytes = 1 }))
            ]
        ]



-- ERROR HANDLING


errorTests : Test
errorTests =
    describe "Errors"
        [ describe "inContext"
            [ test "wraps error with label" <|
                \_ ->
                    BD.decode (BD.inContext "header" (BD.fail "bad")) empty
                        |> Expect.equal (Err (InContext { label = "header", start = 0 } (Custom { at = 0 } "bad")))
            , test "preserves success" <|
                \_ ->
                    BD.decode (BD.inContext "header" BD.unsignedInt8) (fromList [ 1 ])
                        |> Expect.equal (Ok 1)
            , test "nests contexts" <|
                \_ ->
                    BD.decode
                        (BD.inContext "outer"
                            (BD.inContext "inner" (BD.fail "deep"))
                        )
                        empty
                        |> Expect.equal
                            (Err
                                (InContext { label = "outer", start = 0 }
                                    (InContext { label = "inner", start = 0 }
                                        (Custom { at = 0 } "deep")
                                    )
                                )
                            )
            , test "context with sequential decoders" <|
                \_ ->
                    let
                        decoder : BD.Decoder String error ( Int, Int )
                        decoder =
                            BD.inContext "record"
                                (BD.map2 Tuple.pair
                                    (BD.inContext "field1" BD.unsignedInt8)
                                    (BD.inContext "field2" BD.unsignedInt8)
                                )
                    in
                    BD.decode decoder (fromList [ 1 ])
                        |> Expect.equal
                            (Err
                                (InContext { label = "record", start = 0 }
                                    (InContext { label = "field2", start = 1 }
                                        (OutOfBounds { at = 1, bytes = 1 })
                                    )
                                )
                            )
            ]
        ]



-- EDGE CASES


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge cases"
        [ test "decode empty input with succeed" <|
            \_ ->
                BD.decode (BD.succeed ()) empty
                    |> Expect.equal (Ok ())
        , test "exact-length input succeeds" <|
            \_ ->
                BD.decode (BD.map2 Tuple.pair BD.unsignedInt8 BD.unsignedInt8) (fromList [ 1, 2 ])
                    |> Expect.equal (Ok ( 1, 2 ))
        , test "extra bytes are ignored" <|
            \_ ->
                BD.decode BD.unsignedInt8 (fromList [ 42, 99, 100 ])
                    |> Expect.equal (Ok 42)
        , test "large sequential pipeline (6+ fields via nested keep)" <|
            \_ ->
                let
                    decoder : BD.Decoder context error (List Int)
                    decoder =
                        BD.succeed (\a b c d e f -> [ a, b, c, d, e, f ])
                            |> BD.keep BD.unsignedInt8
                            |> BD.keep BD.unsignedInt8
                            |> BD.keep BD.unsignedInt8
                            |> BD.keep BD.unsignedInt8
                            |> BD.keep BD.unsignedInt8
                            |> BD.keep BD.unsignedInt8
                in
                BD.decode decoder (fromList [ 1, 2, 3, 4, 5, 6 ])
                    |> Expect.equal (Ok [ 1, 2, 3, 4, 5, 6 ])
        , test "andThen into applicative sub-decoder" <|
            \_ ->
                let
                    sub : BD.Decoder context error (List Int)
                    sub =
                        BD.map3 (\a b c -> [ a, b, c ]) BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8

                    decoder : BD.Decoder context error (List Int)
                    decoder =
                        BD.unsignedInt8 |> BD.andThen (\_ -> sub)
                in
                BD.decode decoder (fromList [ 0, 10, 20, 30 ])
                    |> Expect.equal (Ok [ 10, 20, 30 ])
        , test "oneOf with applicative alternatives" <|
            \_ ->
                let
                    decoder : BD.Decoder context String String
                    decoder =
                        BD.oneOf
                            [ BD.map2 (\a b -> ( a, b )) BD.unsignedInt8 BD.unsignedInt8
                                |> BD.andThen
                                    (\( a, _ ) ->
                                        if a == 0xFF then
                                            BD.succeed "match"

                                        else
                                            BD.fail "no match"
                                    )
                            , BD.map (\v -> String.fromInt v) BD.unsignedInt8
                            ]
                in
                BD.decode decoder (fromList [ 42 ])
                    |> Expect.equal (Ok "42")
        , test "repeat inside andThen" <|
            \_ ->
                let
                    decoder : BD.Decoder context error (List Int)
                    decoder =
                        BD.unsignedInt8
                            |> BD.andThen (\count -> BD.repeat BD.unsignedInt8 count)
                in
                BD.decode decoder (fromList [ 3, 10, 20, 30 ])
                    |> Expect.equal (Ok [ 10, 20, 30 ])
        , test "skip 0 is no-op in pipeline" <|
            \_ ->
                let
                    decoder : BD.Decoder context error Int
                    decoder =
                        BD.succeed identity
                            |> BD.keep BD.unsignedInt8
                            |> BD.skip 0
                in
                BD.decode decoder (fromList [ 42 ])
                    |> Expect.equal (Ok 42)
        , test "deeply nested succeed" <|
            \_ ->
                let
                    decoder : BD.Decoder context error Int
                    decoder =
                        BD.succeed identity
                            |> BD.keep (BD.succeed identity |> BD.keep (BD.succeed 42))
                in
                BD.decode decoder empty
                    |> Expect.equal (Ok 42)
        , test "oneOf with OutOfBounds collects proper errors" <|
            \_ ->
                let
                    decoder : BD.Decoder context error Int
                    decoder =
                        BD.oneOf
                            [ BD.unsignedInt32 BE
                            , BD.unsignedInt16 BE
                            ]
                in
                BD.decode decoder (fromList [ 1 ])
                    |> Expect.equal
                        (Err
                            (BadOneOf { at = 0 }
                                [ OutOfBounds { at = 0, bytes = 4 }
                                , OutOfBounds { at = 0, bytes = 2 }
                                ]
                            )
                        )
        , test "inContext + oneOf" <|
            \_ ->
                let
                    decoder : BD.Decoder String String value
                    decoder =
                        BD.inContext "value"
                            (BD.oneOf
                                [ BD.fail "not a"
                                , BD.fail "not b"
                                ]
                            )
                in
                BD.decode decoder empty
                    |> Expect.equal
                        (Err
                            (InContext { label = "value", start = 0 }
                                (BadOneOf { at = 0 }
                                    [ Custom { at = 0 } "not a"
                                    , Custom { at = 0 } "not b"
                                    ]
                                )
                            )
                        )
        ]
