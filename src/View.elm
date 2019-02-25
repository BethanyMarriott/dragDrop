module View exposing (view)

import DnDList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model, Items(..))
import Msg exposing (Msg(..))
import List.Extra as List
import Styles
import Config exposing (Fruit)


view : Model -> Html.Html Msg
view model =
    let
        maybeDraggedIndex : Maybe Int
        maybeDraggedIndex =
            Config.system.draggedIndex model.draggable

        groupedItems =
            case model.items of
                Dragging _ updatedList ->
                    updatedList
                NotDragging list ->
                    list
    in
    Html.section
        [ Html.Attributes.style "padding" "3em 0"
        , Html.Attributes.style "text-align" "center"
        ]
        [ groupedItems
            |> List.map (itemView maybeDraggedIndex)
            |> Html.div []
        , draggedItemView model.draggable groupedItems
        ]


groupView : Maybe Int -> List Fruit -> Html.Html Msg
groupView maybeDraggedIndex group =
    group
        |> List.map (itemView maybeDraggedIndex)
        |> Html.div containerStyles


itemView : Maybe Int -> Fruit -> Html.Html Msg
itemView maybeDraggedIndex fruit =
    case maybeDraggedIndex of
        Nothing ->
            Html.div
                [ Html.Attributes.style "margin" "0 2em 3em 2em" ]
                [ Html.div
                    (Html.Attributes.id fruit.id :: itemStyles fruit.group)
                    [ Html.div (handleStyles fruit.group ++ Config.system.dragEvents fruit.position fruit.id) []
                    , Html.div [] [ Html.text fruit.name ]
                    ]
                ]


        Just draggedIndex ->
            if draggedIndex /= fruit.position then
                Html.div
                    [ Html.Attributes.style "margin" "0 2em 3em 2em" ]
                    [ Html.div
                        (itemStyles fruit.group ++ Config.system.dropEvents fruit.position)
                        [ Html.div (handleStyles fruit.group) []
                        , Html.div [] [ Html.text fruit.name ]
                        ]
                    ]

            else
                Html.div
                    [ Html.Attributes.style "margin" "0 2em 3em 2em" ]
                    [ Html.div (itemStyles fruit.group ++ overedItemStyles) [] ]


draggedItemView : DnDList.Draggable -> List Fruit -> Html.Html Msg
draggedItemView draggable fruits =
    let
        maybeDraggedFruit : Maybe Fruit
        maybeDraggedFruit =
            Config.system.draggedIndex draggable
                |> Maybe.andThen (\index -> fruits |> List.drop index |> List.head)
    in
    case maybeDraggedFruit of
        Just fruit ->
            Html.div
                (itemStyles fruit.group ++ draggedItemStyles ++ Config.system.draggedStyles draggable)
                [ Html.div (handleStyles fruit.group ++ draggedHandleStyles) []
                , Html.div [] [ Html.text fruit.name ]
                ]

        Nothing ->
            Html.text ""


    -- STYLES


containerStyles : List (Html.Attribute msg)
containerStyles =
    [ Html.Attributes.style "display" "flex"
    , Html.Attributes.style "flex-wrap" "wrap"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "margin-top" "50px"
    ]


itemStyles : Int -> List (Html.Attribute msg)
itemStyles group =
    [ Html.Attributes.style "width" "180px"
    , Html.Attributes.style "height" "100px"
    , Html.Attributes.style "background" (groupColor group)
    , Html.Attributes.style "border-radius" "8px"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "center"
    ]


draggedItemStyles : List (Html.Attribute msg)
draggedItemStyles =
    [ Html.Attributes.style "background" "#dc9a39" ]


overedItemStyles : List (Html.Attribute msg)
overedItemStyles =
    [ Html.Attributes.style "background" "dimgray" ]


handleStyles : Int -> List (Html.Attribute msg)
handleStyles group =
    [ Html.Attributes.style "width" "50px"
    , Html.Attributes.style "height" "50px"
    , Html.Attributes.style "background" (groupColorDark group)
    , Html.Attributes.style "border-radius" "8px"
    , Html.Attributes.style "margin" "20px"
    , Html.Attributes.style "cursor" "pointer"
    ]


draggedHandleStyles : List (Html.Attribute msg)
draggedHandleStyles =
    [ Html.Attributes.style "background" "#b4752b" ]


groupColor : Int -> String
groupColor group =
    case group of
        0 -> "#cddc39"
        1 -> "#39cddc"
        _ -> "#dc3939"

groupColorDark : Int -> String
groupColorDark group =
    case group of
        0 -> "#afb42b"
        1 -> "#2bafb4"
        _ -> "#b72929"