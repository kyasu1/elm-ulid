module Ulid.Internal exposing
    ( decodeTime
    , fromUuidString
    , isValid
    , multipleUlidStringGenerator
    , toUuidString
    , ulidStringGenerator
    )

import Array exposing (Array)
import Random exposing (Generator, int, list, map, map2)
import Regex
import Time exposing (Posix)


encoding : Array Char
encoding =
    String.toList "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
        |> Array.fromList


base32ToChar : Int -> Char
base32ToChar i =
    Array.get i encoding
        |> Maybe.withDefault 'x'


ulidRegex : Regex.Regex
ulidRegex =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } "[0-9A-FGHJKMNP-TV-Z]"
        |> Maybe.withDefault Regex.never


encodingLength : Int
encodingLength =
    Array.length encoding


isValid : String -> Bool
isValid s =
    if String.length s == 26 && Regex.contains ulidRegex s then
        -- check that the left most char has the code less than 56 which is '8' to detect overflowing
        case s |> String.toList |> List.head |> Maybe.map Char.toCode of
            Just code ->
                if code < 56 then
                    True

                else
                    False

            Nothing ->
                False

    else
        False


ulidStringGenerator : Posix -> Generator String
ulidStringGenerator posix =
    map2 (++) (encodeTime posix |> Random.constant) randomGenerator


multipleUlidStringGenerator : Int -> Posix -> Generator (List String)
multipleUlidStringGenerator num posix =
    let
        time =
            encodeTime posix
    in
    list num (map2 (++) (Random.constant time) randomGenerator)


encodeTime : Posix -> String
encodeTime posix =
    Time.posixToMillis posix
        |> encodeTimeHelper 10
        |> List.reverse
        |> String.fromList


encodeTimeHelper : Int -> Int -> List Char
encodeTimeHelper digits time =
    if digits == 0 then
        []

    else
        let
            mod =
                modBy encodingLength time
        in
        base32ToChar mod :: encodeTimeHelper (digits - 1) (floor <| toFloat (time - mod) / toFloat encodingLength)


decodeTime : String -> Posix
decodeTime s =
    let
        bits =
            String.toList s
                |> List.take 10
                |> List.map base32ToBits
                |> List.concat

        len =
            List.length bits
    in
    bitsToInt len bits
        |> Time.millisToPosix


randomGenerator : Generator String
randomGenerator =
    list 16 base32Generator
        |> map (\list -> List.map base32ToChar list)
        |> map String.fromList


base32Generator : Generator Int
base32Generator =
    int 0 (encodingLength - 1)



-- UUID converter


fromUuidString : String -> Maybe String
fromUuidString uuid =
    uuid
        |> String.replace "-" ""
        |> String.toUpper
        |> String.toList
        |> List.map hexToBits
        |> List.concat
        |> bitsToBaseN 5
        |> List.map base32ToChar
        |> String.fromList
        |> (\ulid ->
                if isValid ulid then
                    Just ulid

                else
                    Nothing
           )


toUuidString : String -> String
toUuidString ulid =
    ulid
        |> String.toList
        |> List.map base32ToBits
        |> List.concat
        |> bitsToBaseN 4
        |> List.drop 1
        |> toUuidStringInternal
        |> String.toLower


toUuidStringInternal : List Int -> String
toUuidStringInternal thirtyTwoHexDigits =
    let
        chars =
            thirtyTwoHexDigits |> List.map base32ToChar
    in
    String.fromList <|
        List.concat
            [ chars |> List.take 8
            , [ '-' ]
            , chars |> List.drop 8 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 12 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 16 |> List.take 4
            , [ '-' ]
            , chars |> List.drop 20 |> List.take 12
            ]



-- Bit operations


bitsToBaseN : Int -> List Int -> List Int
bitsToBaseN baseN bits =
    let
        res =
            modBy baseN (List.length bits)

        padded =
            if res == 0 then
                bits

            else
                List.repeat (baseN - res) 0 ++ bits
    in
    bitsToBaseNHelper baseN padded


bitsToBaseNHelper : Int -> List Int -> List Int
bitsToBaseNHelper baseN bits =
    case bits of
        [] ->
            []

        _ ->
            bitsToInt baseN (List.take baseN bits) :: bitsToBaseNHelper baseN (List.drop baseN bits)


bitsToInt : Int -> List Int -> Int
bitsToInt baseN bits =
    case bits of
        [] ->
            0

        x :: xs ->
            x * (2 ^ (baseN - 1)) + bitsToInt (baseN - 1) xs


hexToBits : Char -> List Int
hexToBits c =
    case c of
        '0' ->
            [ 0, 0, 0, 0 ]

        '1' ->
            [ 0, 0, 0, 1 ]

        '2' ->
            [ 0, 0, 1, 0 ]

        '3' ->
            [ 0, 0, 1, 1 ]

        '4' ->
            [ 0, 1, 0, 0 ]

        '5' ->
            [ 0, 1, 0, 1 ]

        '6' ->
            [ 0, 1, 1, 0 ]

        '7' ->
            [ 0, 1, 1, 1 ]

        '8' ->
            [ 1, 0, 0, 0 ]

        '9' ->
            [ 1, 0, 0, 1 ]

        'A' ->
            [ 1, 0, 1, 0 ]

        'B' ->
            [ 1, 0, 1, 1 ]

        'C' ->
            [ 1, 1, 0, 0 ]

        'D' ->
            [ 1, 1, 0, 1 ]

        'E' ->
            [ 1, 1, 1, 0 ]

        'F' ->
            [ 1, 1, 1, 1 ]

        _ ->
            []


base32ToBits : Char -> List Int
base32ToBits c =
    if Char.isDigit c || (Char.toCode c >= 65 && Char.toCode c <= 70) then
        0 :: hexToBits c

    else
        case c of
            'G' ->
                -- 16
                [ 1, 0, 0, 0, 0 ]

            'H' ->
                -- 17
                [ 1, 0, 0, 0, 1 ]

            'J' ->
                -- 18
                [ 1, 0, 0, 1, 0 ]

            'K' ->
                -- 19
                [ 1, 0, 0, 1, 1 ]

            'M' ->
                -- 20
                [ 1, 0, 1, 0, 0 ]

            'N' ->
                -- 21
                [ 1, 0, 1, 0, 1 ]

            'P' ->
                -- 22
                [ 1, 0, 1, 1, 0 ]

            'Q' ->
                -- 23
                [ 1, 0, 1, 1, 1 ]

            'R' ->
                -- 24
                [ 1, 1, 0, 0, 0 ]

            'S' ->
                -- 25
                [ 1, 1, 0, 0, 1 ]

            'T' ->
                -- 26
                [ 1, 1, 0, 1, 0 ]

            'V' ->
                -- 27
                [ 1, 1, 0, 1, 1 ]

            'W' ->
                -- 28
                [ 1, 1, 1, 0, 0 ]

            'X' ->
                -- 29
                [ 1, 1, 1, 0, 1 ]

            'Y' ->
                -- 30
                [ 1, 1, 1, 1, 0 ]

            'Z' ->
                -- 31
                [ 1, 1, 1, 1, 1 ]

            _ ->
                []
