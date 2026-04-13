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

{-| Dual-path parser: fast path using raw elm/bytes Decoder (no error tracking),
slow path using P2-style State -> ParseResult (full error tracking).

On `run`:

1.  Try the fast Decoder via `Decode.decode`. If it succeeds, return `Ok value`.
2.  If the fast path is unavailable or fails, re-parse with the slow path
    to produce a detailed error.

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


type Parser context error value
    = Parser (Maybe (Decoder value)) (State -> ParseResult context error value)


type ParseResult context error value
    = Good value State
    | Bad (Error context error)


type Error context error
    = InContext { label : context, start : Int } (Error context error)
    | OutOfBounds { at : Int, bytes : Int }
    | Custom { at : Int } error
    | BadOneOf { at : Int } (List (Error context error))


type alias State =
    { offset : Int
    , input : Bytes
    }


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


succeed : value -> Parser context error value
succeed val =
    Parser (Just (Decode.succeed val)) (Good val)


fail : error -> Parser context error value
fail e =
    Parser Nothing (\state -> Bad (Custom { at = state.offset } e))


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


skip : Int -> Parser context error value -> Parser context error value
skip nBytes =
    ignore (bytes nBytes)


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


type Step state a
    = Loop state
    | Done a


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


unsignedInt8 : Parser context error Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


signedInt8 : Parser context error Int
signedInt8 =
    fromDecoder Decode.signedInt8 1


unsignedInt16 : Bytes.Endianness -> Parser context error Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


signedInt16 : Bytes.Endianness -> Parser context error Int
signedInt16 bo =
    fromDecoder (Decode.signedInt16 bo) 2


unsignedInt32 : Bytes.Endianness -> Parser context error Int
unsignedInt32 bo =
    fromDecoder (Decode.unsignedInt32 bo) 4


signedInt32 : Bytes.Endianness -> Parser context error Int
signedInt32 bo =
    fromDecoder (Decode.signedInt32 bo) 4


float32 : Bytes.Endianness -> Parser context error Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


float64 : Bytes.Endianness -> Parser context error Float
float64 bo =
    fromDecoder (Decode.float64 bo) 8


bytes : Int -> Parser context error Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


string : Int -> Parser context error String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount


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
