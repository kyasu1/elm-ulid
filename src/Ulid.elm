module Ulid exposing
    ( Ulid
    , ulidGenerator, multipleUlidGenerator
    , toString, fromString, toTimestamp
    , encode, decode
    , toUuidString, fromUuidString
    )

{-| A library for generating ULID and utility functions.


# Definition

@docs Ulid


# Random Generators

@docs ulidGenerator, multipleUlidGenerator


# Conversion functions

@docs toString, fromString, toTimestamp


# Json encode and decode

@docs encode, decode


# Utility functions for UUID string

@docs toUuidString, fromUuidString

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator, map)
import Time exposing (Posix)
import Ulid.Internal as Internal


{-| opaque type for ULID.

ULID is composed of a Timestamp and a Randomenss part with the following format.

     01AN4Z07BY      79KA1307SR9X4MV3

    |----------|    |----------------|
     Timestamp          Randomness
       48bits             80bits

-}
type Ulid
    = Ulid String


{-| Random generator for a single ULID at specified time.
-}
ulidGenerator : Posix -> Generator Ulid
ulidGenerator posix =
    map Ulid (Internal.ulidStringGenerator posix)


{-| Random generator for multilpe ULIDs at the same milisecond.
The generated ULIDs have no specific sort order.
We can not see use cases for generating ULIDs sortable within the same milisecond on client side apps.
So _Monotonicity_ is not supported in this packages.
-}
multipleUlidGenerator : Int -> Posix -> Generator (List Ulid)
multipleUlidGenerator num posix =
    map (List.map Ulid) (Internal.multipleUlidStringGenerator num posix)


{-| Extract the string representation.
-}
toString : Ulid -> String
toString (Ulid ulid) =
    ulid


{-| Try to parse a string representation as an ULID, ie. 26 character length with valid characters of
`0123456789ABCDEFGHJKMNPQRSTVWXYZ`. Detect overflowing if the ULID is larger than `7ZZZZZZZZZZZZZZZZZZZZZZZZZ`.
-}
fromString : String -> Maybe Ulid
fromString s =
    if Internal.isValid s then
        Just (Ulid s)

    else
        Nothing


{-| Extract the Timestamp part only.
-}
toTimestamp : Ulid -> Posix
toTimestamp (Ulid ulid) =
    ulid |> Internal.decodeTime


{-| Encode a ULID as json
-}
encode : Ulid -> Value
encode (Ulid ulid) =
    JE.string ulid


{-| Decode a ULID from a JSON value
-}
decode : Decoder Ulid
decode =
    JD.string
        |> JD.andThen
            (\s ->
                case fromString s of
                    Just ulid ->
                        JD.succeed ulid

                    Nothing ->
                        JD.fail "Not a valid ULID"
            )


{-| Produce a canonical 8-4-4-4-12 UUID string form.
The variant and version indicators of converted UUID are meaningless,
so the `Uuid.fromString` in [dandy32/elm-uuid] will fail.

[dandy32/elm-uuid]: https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/

-}
toUuidString : Ulid -> String
toUuidString (Ulid ulid) =
    Internal.toUuidString ulid


{-| Try to parse an UUID string, though the timestamp is meaningless.
If it overflows `Nothing` value will be returned.
-}
fromUuidString : String -> Maybe Ulid
fromUuidString uuid =
    uuid
        |> Internal.fromUuidString
        |> Maybe.map Ulid
