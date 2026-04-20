module BenchSkip exposing
    ( default_0
    , default_1
    , default_2
    , default_3
    , default_4
    , default_5
    , default_6
    , default_7
    , default_8
    , default_mixed
    , fast_0
    , fast_1
    , fast_2
    , fast_3
    , fast_4
    , fast_5
    , fast_6
    , fast_7
    , fast_8
    , fast_mixed
    )

{-| Benchmarks for skipBytes on a 256-byte input.

Small chunk sizes (1–8 bytes) are repeated to cover the full 256 bytes.

Prefixes:

  - `default_` — `skipBytesDefault` (`D.map (\_ -> ()) (D.bytes count)`)
  - `fast_` — `skipBytesFast` (uses integer decoders for small sizes)


## Compare approaches — chunk size 0

```sh
elm-bench -f BenchSkip.default_0 -f BenchSkip.fast_0 "()"
```


## Compare approaches — chunk size 1 (×256)

```sh
elm-bench -f BenchSkip.default_1 -f BenchSkip.fast_1 "()"
```


## Compare approaches — chunk size 4 (×64)

```sh
elm-bench -f BenchSkip.default_4 -f BenchSkip.fast_4 "()"
```


## Mixed sizes (0, 2, 5, 7, 16 in sequence)

```sh
elm-bench -f BenchSkip.default_mixed -f BenchSkip.fast_mixed "()"
```

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E



-- TEST DATA


{-| 256 zero bytes.
-}
data256 : Bytes
data256 =
    E.encode (E.sequence (List.repeat 256 (E.unsignedInt8 0)))



-- HELPERS


repeatFast : Int -> D.Decoder value -> D.Decoder ()
repeatFast nTimes dec =
    D.loop nTimes
        (\remaining ->
            if remaining <= 0 then
                D.succeed (D.Done ())

            else
                D.map (\_ -> D.Loop (remaining - 1)) dec
        )



-- SKIP IMPLEMENTATIONS


skipBytesDefault : Int -> D.Decoder ()
skipBytesDefault count =
    D.map (\_ -> ()) (D.bytes count)


skipBytesFast : Int -> D.Decoder ()
skipBytesFast count =
    case count of
        0 ->
            D.succeed ()

        1 ->
            D.map (\_ -> ()) D.unsignedInt8

        2 ->
            D.map (\_ -> ()) (D.unsignedInt16 BE)

        3 ->
            D.map2 (\_ _ -> ()) (D.unsignedInt16 BE) D.unsignedInt8

        4 ->
            D.map (\_ -> ()) (D.unsignedInt32 BE)

        5 ->
            D.map2 (\_ _ -> ()) (D.unsignedInt32 BE) D.unsignedInt8

        6 ->
            D.map2 (\_ _ -> ()) (D.unsignedInt32 BE) (D.unsignedInt16 BE)

        7 ->
            D.map3 (\_ _ _ -> ()) (D.unsignedInt32 BE) (D.unsignedInt16 BE) D.unsignedInt8

        8 ->
            D.map2 (\_ _ -> ()) (D.unsignedInt32 BE) (D.unsignedInt32 BE)

        _ ->
            D.map (\_ -> ()) (D.bytes count)



-- ============================================================================
-- default: skipBytesDefault (D.map (\_ -> ()) (D.bytes count))
-- ============================================================================


default_0 : () -> Maybe ()
default_0 () =
    D.decode (skipBytesDefault 0) data256


default_1 : () -> Maybe ()
default_1 () =
    D.decode (repeatFast 256 (skipBytesDefault 1)) data256


default_2 : () -> Maybe ()
default_2 () =
    D.decode (repeatFast 128 (skipBytesDefault 2)) data256


default_3 : () -> Maybe ()
default_3 () =
    D.decode (repeatFast 85 (skipBytesDefault 3)) data256


default_4 : () -> Maybe ()
default_4 () =
    D.decode (repeatFast 64 (skipBytesDefault 4)) data256


default_5 : () -> Maybe ()
default_5 () =
    D.decode (repeatFast 51 (skipBytesDefault 5)) data256


default_6 : () -> Maybe ()
default_6 () =
    D.decode (repeatFast 42 (skipBytesDefault 6)) data256


default_7 : () -> Maybe ()
default_7 () =
    D.decode (repeatFast 36 (skipBytesDefault 7)) data256


default_8 : () -> Maybe ()
default_8 () =
    D.decode (repeatFast 32 (skipBytesDefault 8)) data256



-- ============================================================================
-- fast: skipBytesFast (uses integer decoders for small sizes)
-- ============================================================================


fast_0 : () -> Maybe ()
fast_0 () =
    D.decode (skipBytesFast 0) data256


fast_1 : () -> Maybe ()
fast_1 () =
    D.decode (repeatFast 256 (skipBytesFast 1)) data256


fast_2 : () -> Maybe ()
fast_2 () =
    D.decode (repeatFast 128 (skipBytesFast 2)) data256


fast_3 : () -> Maybe ()
fast_3 () =
    D.decode (repeatFast 85 (skipBytesFast 3)) data256


fast_4 : () -> Maybe ()
fast_4 () =
    D.decode (repeatFast 64 (skipBytesFast 4)) data256


fast_5 : () -> Maybe ()
fast_5 () =
    D.decode (repeatFast 51 (skipBytesFast 5)) data256


fast_6 : () -> Maybe ()
fast_6 () =
    D.decode (repeatFast 42 (skipBytesFast 6)) data256


fast_7 : () -> Maybe ()
fast_7 () =
    D.decode (repeatFast 36 (skipBytesFast 7)) data256


fast_8 : () -> Maybe ()
fast_8 () =
    D.decode (repeatFast 32 (skipBytesFast 8)) data256



-- ============================================================================
-- mixed: skip sizes 0, 2, 5, 7, 16 in sequence (30 bytes total)
-- ============================================================================


default_mixed : () -> Maybe ()
default_mixed () =
    D.decode
        (D.map5 (\_ _ _ _ _ -> ())
            (skipBytesDefault 0)
            (skipBytesDefault 2)
            (skipBytesDefault 5)
            (skipBytesDefault 7)
            (skipBytesDefault 16)
        )
        data256


fast_mixed : () -> Maybe ()
fast_mixed () =
    D.decode
        (D.map5 (\_ _ _ _ _ -> ())
            (skipBytesFast 0)
            (skipBytesFast 2)
            (skipBytesFast 5)
            (skipBytesFast 7)
            (skipBytesFast 16)
        )
        data256
