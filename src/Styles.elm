module Styles exposing (bordered, spinKeyFrames)

import Css exposing (..)
import Css.Global exposing (global, selector)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)


bordered : Attribute msg
bordered =
    css
        [ border3 (px 1) solid (hex "#000")
        ]


spinKeyFrames : Html msg
spinKeyFrames =
    global
        [ selector "@keyframes spin"
            [ property "0% { transform" "rotate(0deg); } 100% { transform: rotate(360deg); }" ]
        ]
