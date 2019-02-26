module Update exposing (update)

import Array
import List.Extra as List
import Model exposing (Items(..), Model, initialModel)
import Msg exposing (Msg(..))
import Config exposing (Fruit, system)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

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
                                            "id"
                                Nothing ->
                                    "id"
                    in
                    ( { model | draggable = draggable, items = Dragging dragId originalList items }
                    , system.commands model.draggable
                    )

                Dragging dragId originalList updatedList ->
                    let
                        ( draggable, items ) =
                            system.update dndMsg model.draggable updatedList

                        id = Debug.log "id" dragId

                        repositionedList =
                            List.indexedMap (\index scu -> { scu | position = index }) items

                        newItems =
                            case system.draggedIndex model.draggable of
                                Just draggedIndex ->
                                    let
                                        maybeDraggedItem = List.getAt draggedIndex items
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