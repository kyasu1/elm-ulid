module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as JD
import Json.Encode as JE
import Random exposing (initialSeed, step)
import Test exposing (..)
import Time
import Ulid
import Ulid.Internal exposing (..)


time =
    Time.millisToPosix 1558223259095


buildUlid integer =
    let
        initialSeed =
            Random.initialSeed integer

        ( newUlid, seed ) =
            step (Ulid.ulidGenerator time) initialSeed
    in
    newUlid


initialSeedFuzzer =
    Fuzz.map Random.initialSeed Fuzz.int


ulidFuzzer =
    Fuzz.map buildUlid Fuzz.int


suite : Test
suite =
    describe "Testing Ulid module"
        [ test "isValid - for vlaid ulid string" <|
            \_ ->
                Ulid.Internal.isValid "4X0VQ3P73G8AES2YBYBNJ6E15V"
                    |> Expect.true "should be valid"
        , test "isValid - for invalid ulid string" <|
            \_ ->
                Ulid.Internal.isValid "z88duu899"
                    |> Expect.false "should be invalid"
        , test "isValid - for too large ulid" <|
            \_ ->
                Ulid.Internal.isValid "8ZZZZZZZZZZZZZZZZZZZZZZZZZ"
                    |> Expect.false "should be invalid"
        , test "isValid - for the largest ulid" <|
            \_ ->
                Ulid.Internal.isValid "7ZZZZZZZZZZZZZZZZZZZZZZZZZ"
                    |> Expect.true "should be valid"
        , fuzz ulidFuzzer "Testing toTimestamp " <|
            \ulid ->
                ulid
                    |> Ulid.toTimestamp
                    |> Expect.equal time
        , fuzz ulidFuzzer "Converting string then " <|
            \ulid ->
                ulid
                    |> Ulid.toUuidString
                    |> Ulid.fromUuidString
                    |> Expect.equal (Just ulid)
        , fuzz ulidFuzzer "Endoding and decoding" <|
            \ulid ->
                ulid
                    |> Ulid.encode
                    |> JD.decodeValue Ulid.decode
                    |> Expect.equal (Ok ulid)
        , fuzz initialSeedFuzzer "Generate two ulids" <|
            \initialSeed ->
                let
                    ( ulids, seed ) =
                        step (multipleUlidStringGenerator 2 time) initialSeed
                in
                Expect.notEqual (List.take 1 ulids) (List.drop 1 ulids)
        ]
