module Main exposing (main)

import Browser
import Html
import Html.Attributes
import List.Extra as List
import DnDList


-- INIT

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
    system.subscriptions model.draggable


-- CONFIG

type alias Fruit =
    { id : String
    , position : Int
    , group : Int
    , name : String
    }


data : List Fruit
data =
    [ (Fruit "id-Apples" 0 0 "Apples")
    , (Fruit "id-Bananas" 1 0 "Bananas")
    , (Fruit "id-Oranges" 2 0 "Oranges")
    , (Fruit "id-Pears" 3 1 "Pears")
    , (Fruit "id-Cherries" 4 1 "Cherries")
    , (Fruit "id-Grapes" 5 1 "Grapes")
    , (Fruit "id-Plums" 6 2 "Plums")
    , (Fruit "id-Dates" 7 2 "Dates")
    , (Fruit "id-Berries" 6 2 "Berries")
    ]


config : DnDList.Config Msg
config =
    { message = DndMsg
    , movement = DnDList.Free
    }


system : DnDList.System Msg Fruit
system =
    DnDList.create config


-- MODEL

type alias Model =
    { draggable : DnDList.Draggable
    , items : Items
    }

type Items
    = Dragging String (List Fruit) (List Fruit)
    | NotDragging (List Fruit)

initialModel : Model
initialModel =
    { draggable = system.draggable
    , items = NotDragging data
    }


-- MSG

type Msg
    = DndMsg DnDList.Msg


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DndMsg dndMsg ->
             case model.items of
                NotDragging originalList ->
                    let
                        ( draggable, items ) =
                            system.update dndMsg model.draggable originalList

                        dragId =
                            case system.draggedIndex draggable of
                                Just draggedIndex ->
                                    case List.getAt draggedIndex items of
                                        Just item ->
                                            item.id
                                        Nothing ->
                                            ""
                                Nothing ->
                                    ""
                    in
                    ( { model | draggable = draggable, items = Dragging dragId originalList items }
                    , system.commands model.draggable
                    )

                Dragging dragId originalList updatedList ->
                    let
                        ( draggable, items ) =
                            system.update dndMsg model.draggable updatedList

                        repositionedList =
                            List.indexedMap (\index scu -> { scu | position = index }) items

                        newItems =
                            case system.draggedIndex model.draggable of
                                Just draggedIndex ->
                                    let
                                        maybeDraggedItem = List.getAt draggedIndex repositionedList
                                        maybeDroppedAt = List.getAt draggedIndex originalList
                                    in
                                    case (maybeDraggedItem, maybeDroppedAt) of
                                        (Just draggedItem, Just droppedAt) ->
                                            let
                                                regroupedList =
                                                    if draggedItem.id == dragId then
                                                        repositionedList
                                                            |> List.setAt draggedIndex { draggedItem | group = droppedAt.group }
                                                    else
                                                        repositionedList
                                            in
                                            Dragging dragId originalList regroupedList

                                        _ ->
                                            Dragging dragId originalList repositionedList

                                Nothing ->
                                    NotDragging repositionedList
                    in
                    ( { model | draggable = draggable, items = newItems}
                    , system.commands model.draggable
                    )


-- VIEW

view : Model -> Html.Html Msg
view model =
    let
        maybeDraggedIndex : Maybe Int
        maybeDraggedIndex =
            system.draggedIndex model.draggable

        groupedItems =
            case model.items of
                Dragging _ _ updatedList ->
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
                    [ Html.div (handleStyles fruit.group ++ system.dragEvents fruit.position fruit.id) []
                    , Html.div [] [ Html.text fruit.name ]
                    ]
                ]


        Just draggedIndex ->
            if draggedIndex /= fruit.position then
                Html.div
                    [ Html.Attributes.style "margin" "0 2em 3em 2em" ]
                    [ Html.div
                        (itemStyles fruit.group ++ system.dropEvents fruit.position)
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
            system.draggedIndex draggable
                |> Maybe.andThen (\index -> fruits |> List.drop index |> List.head)
    in
    case maybeDraggedFruit of
        Just fruit ->
            Html.div
                (itemStyles fruit.group ++ draggedItemStyles ++ system.draggedStyles draggable)
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