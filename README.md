# Elm ULID

ULID is a 128bit number, designed for *Universally Unique Lexicographically Sortable Identifier*.
For details please refer to the [spec].

This package is an implementation of the ULID generators and utility functions in Elm.

**Note** ULID is NOT compatible with [UUID]s, only common thing is that they are represented in 128-bit number.

Disclaimers:

- The alghorithm for encoding/decoding ULID here is naive and very basic method.
If you have better performant one, we are glad to change to that.
- There is no monotonocity generator which allows generating sortable random number within a same milisecond.
- The randomness provided here is based on the code in [danyx23/elm-uuid],
so the disclaimer in there also applys too.

## Examples

```elm
import Random exposing (Seed, initialSeed, step)
import Task
import Time exposing (Posix)
import Ulid


time =
    Time.millisToPosix 1558339537662


buildUlid integer =
    let
        initialSeed =
            Random.initialSeed integer

        ( newUlid, seed ) =
            step (Ulid.ulidGenerator time) initialSeed
    in
    newUlid


ulid =
    buildUlid 1111    
--> Ulid "01DBA4CJQYGPD539K02D574KC1"


ulidString =
    Ulid.toString ulid
--> "01DBA4CJQYGPD539K02D574KC1"


maybeUlid =
    Ulid.fromString ulidString
--> Just (Ulid "01DBA4CJQYGPD539K02D574KC1")


```

## TODO

- Support the Binary format using [elm/bytes]
- Faster alghorithm

[spec]:https://github.com/ulid/spec
[UUID]:https://en.wikipedia.org/wiki/Universally_unique_identifier
[danyx23/elm-uuid]:https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/
[elm/bytes]:https://package.elm-lang.org/packages/elm/bytes/latest/
