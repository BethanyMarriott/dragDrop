module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Model exposing (Model, initialModel)
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)
import Config


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Config.system.subscriptions model.draggable

