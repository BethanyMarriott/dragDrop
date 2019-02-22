module View exposing (view)

import DnDList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model)
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
            model.items
                |> List.groupWhile (\first second -> first.group == second.group)
                |> List.map (\(item, list) -> item::list)
    in
    Html.section
        [ Html.Attributes.style "padding" "3em 0"
        , Html.Attributes.style "text-align" "center"
        ]
        [ groupedItems
            |> List.map (groupView maybeDraggedIndex)
            |> Html.div []
        , draggedItemView model.draggable model.items
        ]


groupView : Maybe Int -> List Fruit -> Html.Html Msg
groupView maybeDraggedIndex group =
    group
        |> List.map (itemView maybeDraggedIndex)
        |> Html.div [style "margin-top" "140px"]


itemView : Maybe Int -> Fruit -> Html.Html Msg
itemView maybeDraggedIndex item =
    case maybeDraggedIndex of
        Nothing ->
            Html.p
                ([Html.Attributes.id item.id, style "height" "70px"] ++ Config.system.dragEvents item.position item.id)
                [ Html.text item.name ]

        Just draggedIndex ->
            if draggedIndex /= item.position then
                Html.div
                    (style "height" "70px" :: Config.system.dropEvents item.position)
                    [ Html.text item.name ]

            else
                Html.div [style "height" "70px", style "background-color" "#ccc"] [ Html.text "" ]


draggedItemView : DnDList.Draggable -> List Fruit -> Html.Html Msg
draggedItemView draggable items =
    let
        maybeDraggedItem : Maybe Fruit
        maybeDraggedItem =
            Config.system.draggedIndex draggable
                |> Maybe.andThen (\index -> items |> List.drop index |> List.head)
    in
    case maybeDraggedItem of
        Just item ->
            Html.div
                ([style "height" "70px", style "color" "#2b2b2b"] ++ Config.system.draggedStyles draggable)
                [ Html.text item.name ]

        Nothing ->
            Html.text ""