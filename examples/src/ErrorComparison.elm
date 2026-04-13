module ErrorComparison exposing (main)

{-| Side-by-side comparison of error messages from four Elm bytes decoding
approaches, across a variety of failure scenarios.

Build with:

    cd examples
    elm reactor

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Decode.Branchable as BR
import Bytes.Decoder as BD
import Bytes.Encode as E
import Bytes.Parser as ZW
import Html exposing (Html, code, div, h1, h2, h3, p, pre, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)



-- MAIN


main : Html msg
main =
    div [ style "font-family" "system-ui, -apple-system, sans-serif", style "max-width" "1200px", style "margin" "2em auto", style "padding" "0 1em" ]
        [ Html.node "style" [] [ text css ]
        , h1 [] [ text "Bytes Decoder Error Comparison" ]
        , p [ style "color" "#666" ]
            [ text "Each scenario shows what error (if any) each library produces when decoding fails." ]
        , div [] (List.map viewScenario scenarios)
        ]



-- SCENARIO DEFINITION


type alias Scenario =
    { title : String
    , description : String
    , inputDesc : String
    , input : Bytes
    , rawResult : String
    , bdResult : String
    , zwResult : String
    , brResult : String
    }


scenarios : List Scenario
scenarios =
    [ scenario1_emptyInput
    , scenario2_truncatedRecord
    , scenario3_unknownTag
    , scenario4_oneOfExhausted
    , scenario5_nestedContext
    , scenario6_midLoopFailure
    , scenario7_deeplyNested
    ]



-- SCENARIO 1: Reading from empty input


scenario1_emptyInput : Scenario
scenario1_emptyInput =
    let
        input =
            encode []
    in
    { title = "1. Read from empty input"
    , description = "Attempt to decode a single unsignedInt8 from empty bytes."
    , inputDesc = "[] (0 bytes)"
    , input = input
    , rawResult =
        D.decode D.unsignedInt8 input
            |> showMaybe
    , bdResult =
        BD.decode BD.unsignedInt8 input
            |> showBdResult
    , zwResult =
        ZW.run ZW.unsignedInt8 input
            |> showZwResult
    , brResult =
        BR.decode BR.unsignedInt8 input
            |> showMaybe
    }



-- SCENARIO 2: Truncated record (fails on 3rd field of 5)


scenario2_truncatedRecord : Scenario
scenario2_truncatedRecord =
    let
        input =
            encode [ 0x0A, 0x0B ]
    in
    { title = "2. Truncated record (2 of 5 bytes present)"
    , description = "Decode a 5-field record but only 2 bytes are available."
    , inputDesc = "[0x0A, 0x0B] (2 bytes)"
    , input = input
    , rawResult =
        D.decode (D.map5 five D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8 D.unsignedInt8) input
            |> showMaybe
    , bdResult =
        BD.decode
            (BD.map5 five BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
            input
            |> showBdResult
    , zwResult =
        ZW.run
            (ZW.map5 five ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8 ZW.unsignedInt8)
            input
            |> showZwResult
    , brResult =
        BR.decode
            (BR.map5 five BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8 BR.unsignedInt8)
            input
            |> showMaybe
    }



-- SCENARIO 3: Unknown tag in andThen dispatch


scenario3_unknownTag : Scenario
scenario3_unknownTag =
    let
        input =
            encode [ 0xFF, 0x01, 0x02 ]
    in
    { title = "3. Unknown tag byte"
    , description = "Read a tag byte, then dispatch. Tag 0xFF is unrecognized."
    , inputDesc = "[0xFF, 0x01, 0x02] (3 bytes)"
    , input = input
    , rawResult =
        D.decode (D.unsignedInt8 |> D.andThen (rawTagDispatch)) input
            |> showMaybe
    , bdResult =
        BD.decode (BD.unsignedInt8 |> BD.andThen (bdTagDispatch)) input
            |> showBdResult
    , zwResult =
        ZW.run (ZW.unsignedInt8 |> ZW.andThen (zwTagDispatch)) input
            |> showZwResult
    , brResult =
        BR.decode (BR.unsignedInt8 |> BR.andThen (brTagDispatch)) input
            |> showMaybe
    }



-- SCENARIO 4: oneOf with all alternatives exhausted


scenario4_oneOfExhausted : Scenario
scenario4_oneOfExhausted =
    let
        input =
            encode [ 0x99 ]
    in
    { title = "4. oneOf — all alternatives fail"
    , description = "Try three tag-based alternatives; byte 0x99 matches none."
    , inputDesc = "[0x99] (1 byte)"
    , input = input
    , rawResult = "N/A (elm/bytes has no oneOf)"
    , bdResult =
        BD.decode bdTagOneOf input
            |> showBdResult
    , zwResult =
        ZW.run zwTagOneOf input
            |> showZwResult
    , brResult =
        BR.decode brTagOneOf input
            |> showMaybe
    }



-- SCENARIO 5: Nested context labels


scenario5_nestedContext : Scenario
scenario5_nestedContext =
    let
        input =
            encode [ 0x01 ]
    in
    { title = "5. Nested context labels"
    , description = "Decode a \"transaction\" containing an \"output\" containing a uint16, but only 1 byte remains after reading the first byte."
    , inputDesc = "[0x01] (1 byte)"
    , input = input
    , rawResult =
        D.decode (D.unsignedInt8 |> D.andThen (\_ -> D.unsignedInt16 BE)) input
            |> showMaybe
    , bdResult =
        BD.decode
            (BD.inContext "transaction"
                (BD.unsignedInt8
                    |> BD.andThen
                        (\_ ->
                            BD.inContext "output"
                                (BD.unsignedInt16 BE)
                        )
                )
            )
            input
            |> showBdResult
    , zwResult =
        ZW.run
            (ZW.inContext "transaction"
                (ZW.unsignedInt8
                    |> ZW.andThen
                        (\_ ->
                            ZW.inContext "output"
                                (ZW.unsignedInt16 BE)
                        )
                )
            )
            input
            |> showZwResult
    , brResult = "N/A (Branchable has no inContext)"
    }



-- SCENARIO 6: Failure mid-loop


scenario6_midLoopFailure : Scenario
scenario6_midLoopFailure =
    let
        -- 3 bytes but we try to read 5 uint8s
        input =
            encode [ 0x0A, 0x0B, 0x0C ]
    in
    { title = "6. Failure mid-loop (3 of 5 items)"
    , description = "Loop to decode 5 unsignedInt8 values, but only 3 bytes available."
    , inputDesc = "[0x0A, 0x0B, 0x0C] (3 bytes)"
    , input = input
    , rawResult =
        D.decode (rawRepeat5 D.unsignedInt8) input
            |> showMaybe
    , bdResult =
        BD.decode (BD.repeat BD.unsignedInt8 5) input
            |> showBdResult
    , zwResult =
        ZW.run (ZW.repeat ZW.unsignedInt8 5) input
            |> showZwResult
    , brResult =
        BR.decode (BR.repeat BR.unsignedInt8 5) input
            |> showMaybe
    }



-- SCENARIO 7: Deeply nested oneOf with context


scenario7_deeplyNested : Scenario
scenario7_deeplyNested =
    let
        -- A "message" with format byte, then a "payload" that doesn't match any known type
        input =
            encode [ 0x01, 0xFF, 0xAB ]
    in
    { title = "7. Deeply nested oneOf with context"
    , description = "A message header byte, then a payload where tag 0xFF matches no known format."
    , inputDesc = "[0x01, 0xFF, 0xAB] (3 bytes)"
    , input = input
    , rawResult = "N/A (elm/bytes has no oneOf or context)"
    , bdResult =
        BD.decode
            (BD.inContext "message"
                (BD.unsignedInt8
                    |> BD.andThen
                        (\_ ->
                            BD.inContext "payload" bdTagOneOf
                        )
                )
            )
            input
            |> showBdResult
    , zwResult =
        ZW.run
            (ZW.inContext "message"
                (ZW.unsignedInt8
                    |> ZW.andThen
                        (\_ ->
                            ZW.inContext "payload" zwTagOneOf
                        )
                )
            )
            input
            |> showZwResult
    , brResult = "N/A (Branchable has no inContext)"
    }



-- SHARED DECODERS


five : Int -> Int -> Int -> Int -> Int -> List Int
five a b c d e =
    [ a, b, c, d, e ]


rawTagDispatch : Int -> D.Decoder String
rawTagDispatch tag =
    case tag of
        0 ->
            D.map String.fromInt D.unsignedInt8

        1 ->
            D.string 2

        _ ->
            D.fail


bdTagDispatch : Int -> BD.Decoder c String String
bdTagDispatch tag =
    case tag of
        0 ->
            BD.map String.fromInt BD.unsignedInt8

        1 ->
            BD.string 2

        _ ->
            BD.fail ("unknown tag: " ++ String.fromInt tag ++ " (expected 0 or 1)")


zwTagDispatch : Int -> ZW.Parser String String String
zwTagDispatch tag =
    case tag of
        0 ->
            ZW.map String.fromInt ZW.unsignedInt8

        1 ->
            ZW.string 2

        _ ->
            ZW.fail ("unknown tag: " ++ String.fromInt tag ++ " (expected 0 or 1)")


brTagDispatch : Int -> BR.Decoder String
brTagDispatch tag =
    case tag of
        0 ->
            BR.map String.fromInt BR.unsignedInt8

        1 ->
            BR.string 2

        _ ->
            BR.fail


bdTagOneOf : BD.Decoder c String Int
bdTagOneOf =
    BD.oneOf
        [ BD.unsignedInt8 |> BD.andThen (\t -> if t == 0 then BD.unsignedInt8 else BD.fail ("tag " ++ String.fromInt t ++ " /= 0"))
        , BD.unsignedInt8 |> BD.andThen (\t -> if t == 1 then BD.unsignedInt16 BE else BD.fail ("tag " ++ String.fromInt t ++ " /= 1"))
        , BD.unsignedInt8 |> BD.andThen (\t -> if t == 2 then BD.unsignedInt32 BE else BD.fail ("tag " ++ String.fromInt t ++ " /= 2"))
        ]


zwTagOneOf : ZW.Parser String String Int
zwTagOneOf =
    ZW.oneOf
        [ ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 0 then ZW.unsignedInt8 else ZW.fail ("tag " ++ String.fromInt t ++ " /= 0"))
        , ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 1 then ZW.unsignedInt16 BE else ZW.fail ("tag " ++ String.fromInt t ++ " /= 1"))
        , ZW.unsignedInt8 |> ZW.andThen (\t -> if t == 2 then ZW.unsignedInt32 BE else ZW.fail ("tag " ++ String.fromInt t ++ " /= 2"))
        ]


brTagOneOf : BR.Decoder Int
brTagOneOf =
    BR.oneOf
        [ BR.unsignedInt8 |> BR.andThen (\t -> if t == 0 then BR.unsignedInt8 else BR.fail)
        , BR.unsignedInt8 |> BR.andThen (\t -> if t == 1 then BR.unsignedInt16 BE else BR.fail)
        , BR.unsignedInt8 |> BR.andThen (\t -> if t == 2 then BR.unsignedInt32 BE else BR.fail)
        ]


rawRepeat5 : D.Decoder a -> D.Decoder (List a)
rawRepeat5 dec =
    D.loop ( 5, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                D.succeed (D.Done (List.reverse acc))

            else
                D.map (\v -> D.Loop ( remaining - 1, v :: acc )) dec
        )



-- ENCODING HELPERS


encode : List Int -> Bytes
encode ints =
    E.encode (E.sequence (List.map E.unsignedInt8 ints))



-- DISPLAY HELPERS


showMaybe : Maybe a -> String
showMaybe m =
    case m of
        Just v ->
            "Ok: " ++ Debug.toString v

        Nothing ->
            "Nothing\n  (no offset, no error message, no context)"


showBdResult : Result (BD.Error c e) a -> String
showBdResult r =
    case r of
        Ok v ->
            "Ok: " ++ Debug.toString v

        Err e ->
            showBdError 0 e


showBdError : Int -> BD.Error c e -> String
showBdError indent err =
    let
        pad =
            String.repeat (indent * 2) " "
    in
    case err of
        BD.OutOfBounds { at, bytes } ->
            pad ++ "OutOfBounds { at = " ++ String.fromInt at ++ ", bytes = " ++ String.fromInt bytes ++ " }"

        BD.Custom { at } e ->
            pad ++ "Custom (at " ++ String.fromInt at ++ ") " ++ Debug.toString e

        BD.BadOneOf { at } errors ->
            pad
                ++ "BadOneOf (at "
                ++ String.fromInt at
                ++ "):\n"
                ++ String.join "\n" (List.indexedMap (\i e -> pad ++ "  [" ++ String.fromInt (i + 1) ++ "] " ++ showBdError 0 e) errors)

        BD.InContext { label, start } inner ->
            pad ++ "InContext { label = " ++ Debug.toString label ++ ", start = " ++ String.fromInt start ++ " }:\n" ++ showBdError (indent + 1) inner


showZwResult : Result (ZW.Error c e) a -> String
showZwResult r =
    case r of
        Ok v ->
            "Ok: " ++ Debug.toString v

        Err e ->
            showZwError 0 e


showZwError : Int -> ZW.Error c e -> String
showZwError indent err =
    let
        pad =
            String.repeat (indent * 2) " "
    in
    case err of
        ZW.OutOfBounds { at, bytes } ->
            pad ++ "OutOfBounds { at = " ++ String.fromInt at ++ ", bytes = " ++ String.fromInt bytes ++ " }"

        ZW.Custom { at } e ->
            pad ++ "Custom (at " ++ String.fromInt at ++ ") " ++ Debug.toString e

        ZW.BadOneOf { at } errors ->
            pad
                ++ "BadOneOf (at "
                ++ String.fromInt at
                ++ "):\n"
                ++ String.join "\n" (List.indexedMap (\i e -> pad ++ "  [" ++ String.fromInt (i + 1) ++ "] " ++ showZwError 0 e) errors)

        ZW.InContext { label, start } inner ->
            pad ++ "InContext { label = " ++ Debug.toString label ++ ", start = " ++ String.fromInt start ++ " }:\n" ++ showZwError (indent + 1) inner



-- VIEW


viewScenario : Scenario -> Html msg
viewScenario s =
    div [ class "scenario" ]
        [ h2 [] [ text s.title ]
        , p [] [ text s.description ]
        , p []
            [ text "Input: "
            , code [] [ text s.inputDesc ]
            ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "elm/bytes" ]
                    , th [] [ text "elm-cardano/bytes-decoder" ]
                    , th [] [ text "zwilias/elm-bytes-parser" ]
                    , th [] [ text "mpizenberg/Branchable" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ pre [] [ text s.rawResult ] ]
                    , td [] [ pre [] [ text s.bdResult ] ]
                    , td [] [ pre [] [ text s.zwResult ] ]
                    , td [] [ pre [] [ text s.brResult ] ]
                    ]
                ]
            ]
        ]



-- CSS


css : String
css =
    """
body { background: #fafafa; color: #222; }
h1 { border-bottom: 2px solid #333; padding-bottom: 0.3em; }
.scenario { margin-bottom: 2.5em; }
h2 { color: #1a1a2e; margin-bottom: 0.2em; }
table { width: 100%; border-collapse: collapse; margin-top: 0.5em; }
th { background: #e8e8e8; text-align: left; padding: 0.5em 0.7em; border: 1px solid #ccc; font-size: 0.85em; }
td { vertical-align: top; padding: 0; border: 1px solid #ccc; width: 25%; }
td pre { margin: 0; padding: 0.5em 0.7em; font-size: 0.8em; white-space: pre-wrap; word-break: break-word; background: #fff; min-height: 3em; }
code { background: #eee; padding: 0.1em 0.4em; border-radius: 3px; font-size: 0.9em; }
"""
