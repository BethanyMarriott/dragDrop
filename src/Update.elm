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
            case maybeDraggedIndex of
                Just draggedIndex ->
                    let
                        return =
                            case model.items of
                                NotDragging originalList ->
                                    ( { model | items = Dragging originalList originalList }
                                    , Cmd.none
                                    )

                                Dragging originalList updatedList ->
                                    let
                                        ( draggable, items ) =
                                            system.update dndMsg model.draggable updatedList
                                    in
                                    ( { model | draggable = draggable, items = (Dragging originalList items) }
                                    , system.commands model.draggable
                                    )
                    in
                    return
                Nothing ->
                    ( model
                    , Cmd.none
                    )



getIndexOf : Maybe Int -> Fruit -> (Int, Bool) -> (Int, Bool)
getIndexOf maybeDragId fruit (index, shouldAcc) =
    case shouldAcc of
        True ->
            if Just fruit.position == maybeDragId then
                (index, False)
            else
                (index + 1, True)
        False ->
            (index, shouldAcc)