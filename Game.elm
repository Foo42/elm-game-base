module Game exposing (..)

import Html exposing (Html, text)
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


type alias Model =
    { accelleration : Float
    , velocity : Float
    , position : Float
    , shotsFired : Int
    }


model : Model
model =
    { accelleration = 0
    , velocity = 0
    , position = 0
    , shotsFired = 0
    }


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
            incrementShotsFired model

        ArrowLeft ->
            updateAcceleration -1.0 model

        ArrowRight ->
            updateAcceleration 1.0 model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateAcceleration 0 model

        ArrowRight ->
            updateAcceleration 0 model

        _ ->
            model

progressTime : Float -> Model -> Model
progressTime dt model =
    model
    |> applySecondDerivatives dt
    |> applyFirstDerivatives dt

applySecondDerivatives: Float -> Model -> Model
applySecondDerivatives dt model =
    { model | velocity = model.velocity + model.accelleration * dt }

applyFirstDerivatives: Float -> Model -> Model
applyFirstDerivatives dt model =
    { model | position = model.position + model.velocity * dt }

updateAcceleration : Float -> Model -> Model
updateAcceleration newAcceleration model =
    { model | accelleration = newAcceleration }


incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    text (toString model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
