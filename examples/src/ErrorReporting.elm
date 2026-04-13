module ErrorReporting exposing (main)

{-| Examples of error reporting with Bytes.Decoder.

Demonstrates the four error types: `OutOfBounds`, `Custom`, `BadOneOf`,
and `InContext`, across a variety of failure scenarios.

Build with:

```sh
cd examples
elm reactor
```

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decoder as BD
import Bytes.Encode as E
import Html exposing (Html, code, div, h1, h2, p, pre, text)
import Html.Attributes exposing (style)



-- MAIN


main : Html msg
main =
    div [ style "font-family" "system-ui, -apple-system, sans-serif", style "max-width" "900px", style "margin" "2em auto", style "padding" "0 1em" ]
        [ Html.node "style" [] [ text css ]
        , h1 [] [ text "Bytes.Decoder Error Reporting" ]
        , p [ style "color" "#666" ]
            [ text "Each scenario demonstrates how Bytes.Decoder reports errors when decoding fails." ]
        , div [] (List.map viewScenario scenarios)
        ]



-- SCENARIO DEFINITION


type alias Scenario =
    { title : String
    , description : String
    , inputDesc : String
    , result : String
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
    { title = "1. OutOfBounds — read from empty input"
    , description = "Attempt to decode a single unsignedInt8 from empty bytes."
    , inputDesc = "[] (0 bytes)"
    , result =
        BD.decode BD.unsignedInt8 input
            |> showResult
    }



-- SCENARIO 2: Truncated record (fails on 3rd field of 5)


scenario2_truncatedRecord : Scenario
scenario2_truncatedRecord =
    let
        input =
            encode [ 0x0A, 0x0B ]
    in
    { title = "2. OutOfBounds — truncated record (2 of 5 bytes present)"
    , description = "Decode a 5-field record but only 2 bytes are available."
    , inputDesc = "[0x0A, 0x0B] (2 bytes)"
    , result =
        BD.decode
            (BD.map5 five BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8 BD.unsignedInt8)
            input
            |> showResult
    }



-- SCENARIO 3: Unknown tag in andThen dispatch


scenario3_unknownTag : Scenario
scenario3_unknownTag =
    let
        input =
            encode [ 0xFF, 0x01, 0x02 ]
    in
    { title = "3. Custom error — unknown tag byte"
    , description = "Read a tag byte, then dispatch. Tag 0xFF is unrecognized, so we fail with a custom error message."
    , inputDesc = "[0xFF, 0x01, 0x02] (3 bytes)"
    , result =
        BD.decode (BD.unsignedInt8 |> BD.andThen tagDispatch) input
            |> showResult
    }



-- SCENARIO 4: oneOf with all alternatives exhausted


scenario4_oneOfExhausted : Scenario
scenario4_oneOfExhausted =
    let
        input =
            encode [ 0x99 ]
    in
    { title = "4. BadOneOf — all alternatives fail"
    , description = "Try three tag-based alternatives; byte 0x99 matches none."
    , inputDesc = "[0x99] (1 byte)"
    , result =
        BD.decode tagOneOf input
            |> showResult
    }



-- SCENARIO 5: Nested context labels


scenario5_nestedContext : Scenario
scenario5_nestedContext =
    let
        input =
            encode [ 0x01 ]
    in
    { title = "5. InContext — nested context labels"
    , description = "Decode a \"transaction\" containing an \"output\" containing a uint16, but only 1 byte remains after reading the first byte."
    , inputDesc = "[0x01] (1 byte)"
    , result =
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
            |> showResult
    }



-- SCENARIO 6: Failure mid-loop


scenario6_midLoopFailure : Scenario
scenario6_midLoopFailure =
    let
        input =
            encode [ 0x0A, 0x0B, 0x0C ]
    in
    { title = "6. OutOfBounds — failure mid-loop (3 of 5 items)"
    , description = "Loop to decode 5 unsignedInt8 values, but only 3 bytes available."
    , inputDesc = "[0x0A, 0x0B, 0x0C] (3 bytes)"
    , result =
        BD.decode (BD.repeat BD.unsignedInt8 5) input
            |> showResult
    }



-- SCENARIO 7: Deeply nested oneOf with context


scenario7_deeplyNested : Scenario
scenario7_deeplyNested =
    let
        input =
            encode [ 0x01, 0xFF, 0xAB ]
    in
    { title = "7. Combined — deeply nested oneOf with context"
    , description = "A message header byte, then a payload where tag 0xFF matches no known format. Shows InContext wrapping a BadOneOf containing Custom errors."
    , inputDesc = "[0x01, 0xFF, 0xAB] (3 bytes)"
    , result =
        BD.decode
            (BD.inContext "message"
                (BD.unsignedInt8
                    |> BD.andThen
                        (\_ ->
                            BD.inContext "payload" tagOneOf
                        )
                )
            )
            input
            |> showResult
    }



-- SHARED DECODERS


five : Int -> Int -> Int -> Int -> Int -> List Int
five a b c d e =
    [ a, b, c, d, e ]


tagDispatch : Int -> BD.Decoder c String String
tagDispatch tag =
    case tag of
        0 ->
            BD.map String.fromInt BD.unsignedInt8

        1 ->
            BD.string 2

        _ ->
            BD.fail ("unknown tag: " ++ String.fromInt tag ++ " (expected 0 or 1)")


tagOneOf : BD.Decoder c String Int
tagOneOf =
    BD.oneOf
        [ BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 0 then
                        BD.unsignedInt8

                    else
                        BD.fail ("tag " ++ String.fromInt t ++ " /= 0")
                )
        , BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 1 then
                        BD.unsignedInt16 BE

                    else
                        BD.fail ("tag " ++ String.fromInt t ++ " /= 1")
                )
        , BD.unsignedInt8
            |> BD.andThen
                (\t ->
                    if t == 2 then
                        BD.unsignedInt32 BE

                    else
                        BD.fail ("tag " ++ String.fromInt t ++ " /= 2")
                )
        ]



-- ENCODING HELPERS


encode : List Int -> Bytes
encode ints =
    E.encode (E.sequence (List.map E.unsignedInt8 ints))



-- DISPLAY HELPERS


showResult : Result (BD.Error c e) a -> String
showResult r =
    case r of
        Ok v ->
            "Ok: " ++ Debug.toString v

        Err e ->
            showError 0 e


showError : Int -> BD.Error c e -> String
showError indent err =
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
                ++ String.join "\n" (List.indexedMap (\i e -> pad ++ "  [" ++ String.fromInt (i + 1) ++ "] " ++ showError 0 e) errors)

        BD.InContext { label, start } inner ->
            pad ++ "InContext { label = " ++ Debug.toString label ++ ", start = " ++ String.fromInt start ++ " }:\n" ++ showError (indent + 1) inner



-- VIEW


viewScenario : Scenario -> Html msg
viewScenario s =
    div [ style "margin-bottom" "2.5em" ]
        [ h2 [] [ text s.title ]
        , p [] [ text s.description ]
        , p []
            [ text "Input: "
            , code [ style "background" "#eee", style "padding" "0.1em 0.4em", style "border-radius" "3px", style "font-size" "0.9em" ] [ text s.inputDesc ]
            ]
        , pre
            [ style "background" "#f5f5f5"
            , style "border" "1px solid #ddd"
            , style "border-radius" "4px"
            , style "padding" "0.8em 1em"
            , style "font-size" "0.85em"
            , style "white-space" "pre-wrap"
            , style "word-break" "break-word"
            ]
            [ text s.result ]
        ]



-- CSS


css : String
css =
    """
body { background: #fafafa; color: #222; }
h1 { border-bottom: 2px solid #333; padding-bottom: 0.3em; }
h2 { color: #1a1a2e; margin-bottom: 0.2em; }
"""
