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
            let
                maybeDraggedIndex = system.draggedIndex model.draggable
--
--                ( draggable, items ) =
--                    system.update dndMsg model.draggable model.items
--
--
--                reindexedItems =
--                    items
--                        |> List.indexedMap (\index item -> { item | position = index })




--                (droppedAtIndex, shouldAcc) =
--                    List.foldl (getDroppedAt maybeDragId) (0, True) items
--
--                updatedItems =
--                    case shouldAcc of
--                        False ->
--                            let
--                                maybeDraggedItem =
--                                    reindexedItems
--                                        |> List.foldl (\scu acc -> if scu.position == droppedAtIndex then scu :: acc else acc) []
--                                        |> List.head
--
--                                maybeDroppedItem =
--                                    items
--                                        |> List.foldl (\scu acc -> if scu.position == droppedAtIndex then scu :: acc else acc) []
--                                        |> List.head
--
--                            in
--                            case (maybeDraggedItem, maybeDroppedItem) of
--                                (Just draggedItem, Just droppedItem) ->
--                                    let
--                                        updatedItem =
--                                            { draggedItem | group = droppedItem.group }
--
--                                    in
--                                    reindexedItems
--                                        |> Array.fromList
--                                        |> Array.set droppedAtIndex updatedItem
--                                        |> Array.toList
--
--                                _ ->
--                                    reindexedItems
--                        True ->
--                            reindexedItems


            in
             case model.items of
                NotDragging originalList ->
                    let
                        ( draggable, items ) =
                            system.update dndMsg model.draggable originalList

                    in
                    ( { model | draggable = draggable, items = Dragging originalList items }
                    , system.commands model.draggable
                    )

                Dragging originalList updatedList ->
                    let
                        ( draggable, items ) =
                            system.update dndMsg model.draggable updatedList

                        repositionedList =
                            List.indexedMap (\index scu -> { scu | position = index }) items

                        newItems =
                            case system.draggedIndex model.draggable of
                                Just draggedIndex ->
                                    let
                                        dragged = Debug.log "dragged" draggedIndex
                                        maybeDraggedItem = List.getAt draggedIndex repositionedList |> Debug.log "hi"
                                        maybeDroppedAt = List.getAt draggedIndex originalList
                                    in
                                    case (maybeDraggedItem, maybeDroppedAt) of
                                        (Just draggedItem, Just droppedAt) ->
                                            let
                                                regroupedList =
                                                    if draggedItem.position == draggedIndex then
                                                    Array.fromList repositionedList
                                                        |> Array.set draggedIndex { draggedItem | group = droppedAt.group }
                                                        |> Array.toList
                                                    else
                                                    repositionedList
                                            in
                                            Dragging originalList regroupedList
                                        _ ->
                                            Dragging originalList repositionedList

                                Nothing ->
                                    NotDragging repositionedList
                    in
                    ( { model | draggable = draggable, items = newItems}
                    , system.commands model.draggable
                    )



getIndexOf : Maybe Fruit -> Fruit -> (Int, Bool) -> (Int, Bool)
getIndexOf maybeFruit fruit (index, shouldAcc) =
    case shouldAcc of
        True ->
            if True then
                (index, False)
            else
                (index + 1, True)
        False ->
            (index, shouldAcc)