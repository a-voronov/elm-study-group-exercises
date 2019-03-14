module Timer exposing (Constants, Model, Msg(..), constants, init, initialModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { duration : Int
    , startingTime : Maybe Int
    , currentTime : Maybe Int
    }


initialModel =
    Model constants.min Nothing Nothing


type alias Constants =
    { min : Int, max : Int, step : Int, milliseconds : Int }


constants =
    Constants 1 30 1 1000


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Task.perform RestartTime Time.now )



-- UPDATE


type Msg
    = Reset
    | RestartTime Time.Posix
    | UpdateCurrentTime Time.Posix
    | UpdateDuration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { initialModel | duration = model.duration }, Task.perform RestartTime Time.now )

        RestartTime time ->
            ( { model | startingTime = Just <| Time.posixToMillis time }, Cmd.none )

        UpdateCurrentTime timePosix ->
            let
                time =
                    Time.posixToMillis timePosix

                duration =
                    model.duration * constants.milliseconds

                elapsed currentTime startingTime =
                    currentTime - startingTime

                updateTimes currentTime startingTime =
                    case ( currentTime, startingTime ) of
                        ( Just ct, Just st ) ->
                            if elapsed ct st < duration then
                                -- if elapsed time hasn't reached duration,
                                -- update only current time
                                ( Just time, startingTime )

                            else
                                -- keep starting time always up-to-date with current time,
                                -- so that next time we move slider, it continues from the point we left
                                ( Just time, Just <| time - duration )

                        _ ->
                            ( Just time, model.startingTime )

                ( current, starting ) =
                    updateTimes model.currentTime model.startingTime
            in
            ( { model | currentTime = current, startingTime = starting }, Cmd.none )

        UpdateDuration duration ->
            ( { model | duration = Maybe.withDefault constants.min (String.toInt duration) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (toFloat <| constants.step * constants.milliseconds) UpdateCurrentTime



-- VIEW


view : Model -> Html Msg
view model =
    let
        timeProgress =
            case ( model.currentTime, model.startingTime ) of
                ( Just currentTime, Just startingTime ) ->
                    -- calculating percentage of elapsed time
                    round <| toFloat (currentTime - startingTime) / toFloat constants.milliseconds / toFloat model.duration * 100

                _ ->
                    0
    in
    div []
        [ div []
            [ progress
                [ A.max "100"
                , value <| String.fromInt timeProgress
                ]
                []
            ]
        , input
            [ type_ "range"
            , A.min <| String.fromInt constants.min
            , A.max <| String.fromInt constants.max
            , value <| String.fromInt model.duration
            , onInput UpdateDuration
            ]
            []
        , text <| String.fromFloat (toFloat model.duration) ++ "s"
        , div []
            [ input
                [ type_ "button"
                , value "Reset"
                , onClick Reset
                ]
                []
            ]
        ]
