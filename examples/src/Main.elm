module Main exposing (main)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed, step)
import Task
import Time exposing (Posix)
import Ulid


type alias Model =
    { currentSeed : Seed
    , currentUlid : Maybe Ulid.Ulid
    }


type Msg
    = ClickedButton
    | GotCurrentTime Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedButton ->
            ( model, Task.perform GotCurrentTime Time.now )

        GotCurrentTime posix ->
            let
                ( newUlid, newSeed ) =
                    step (Ulid.ulidGenerator posix) model.currentSeed
            in
            ( { model | currentSeed = newSeed, currentUlid = Just newUlid }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ClickedButton ] [ text "Create a new ULID !" ]
        , case model.currentUlid of
            Just ulid ->
                div []
                    [ div []
                        [ text "Time Stamp : "
                        , text <| String.fromInt <| Time.posixToMillis (Ulid.toTimestamp ulid)
                        ]
                    , div
                        []
                        [ text "Generated ULID : "
                        , span [ style "color" "blue" ] [ text <| String.left 10 <| Ulid.toString ulid ]
                        , span [ style "color" "green" ] [ text <| String.dropLeft 10 <| Ulid.toString ulid ]
                        ]
                    ]

            Nothing ->
                div [] [ text "No ULID was created so far" ]
        ]


init : Int -> ( Model, Cmd Msg )
init seed =
    ( { currentSeed = initialSeed seed
      , currentUlid = Nothing
      }
    , Cmd.none
    )


main : Program Int Model Msg
main =
    element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
