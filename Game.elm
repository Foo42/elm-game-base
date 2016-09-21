module Game exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL

type alias Physical t =
    {t | accelleration : Float
    , velocity : Float
    , position : Float}

type alias Player = Physical { shotsFired : Int }

player : Player
player =
    { accelleration = 0
    , velocity = 0
    , position = 0
    , shotsFired = 0
    }

type alias Model =
    { player: Player
    }

model : Model
model =
    {player = player}

init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( progressTime dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            {model | player = incrementShotsFired model.player}

        ArrowLeft ->
            {model | player = updateAcceleration -1.0 model.player}

        ArrowRight ->
            {model | player = updateAcceleration 1.0 model.player}

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            {model | player = updateAcceleration 0 model.player}

        ArrowRight ->
            {model | player = updateAcceleration 0 model.player}

        _ ->
            model

progressTime : Float -> Model -> Model
progressTime dt model =
    let updatedPlayer = (model.player |> (applySecondDerivatives dt) |> (applyFirstDerivatives dt) )
    in
        {model | player = updatedPlayer}

applySecondDerivatives: Float -> Physical t -> Physical t
applySecondDerivatives dt physical =
    { physical | velocity = physical.velocity + physical.accelleration * dt - (drag dt physical.velocity) }

drag : Float -> Float -> Float
drag dt velocity =
    velocity * dt/1000

applyFirstDerivatives: Float -> Physical t -> Physical t
applyFirstDerivatives dt physical =
    { physical | position = physical.position + physical.velocity * dt }

updateAcceleration : Float -> Physical t -> Physical t
updateAcceleration newAcceleration physical =
    { physical | accelleration = newAcceleration }


incrementShotsFired : Player -> Player
incrementShotsFired player =
    { player | shotsFired = player.shotsFired + 1 }



-- VIEW

calculateLeftPercentage : Physical t -> String
calculateLeftPercentage physical =
    (toString (physical.position / 5000 + 50)) ++ "%"

view : Model -> Html msg
view model =
    div
        [style [("height", "100%"), ("width", "100%"), ("background-color", "black"), ("position", "relative")]]
        [
            text (toString model),
            div
            [
                style [("height", "1%"), ("width", "1%"), ("background-color", "white"), ("position", "absolute"), ("left", calculateLeftPercentage model.player)]
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
