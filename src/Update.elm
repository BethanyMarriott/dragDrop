module Update exposing (update)

import Array
import Model exposing (Model, initialModel)
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
                ( draggable, items ) =
                    system.update dndMsg model.draggable model.items


                oldItems = model.items

                maybeDragId = system.draggedIndex draggable

                reindexedItems =
                    items
                        |> List.indexedMap (\index item -> { item | position = index })

                (droppedAtIndex, shouldAcc) =
                    List.foldl (getDroppedAt maybeDragId) (0, True) items |> Debug.log "dropped at"

                updatedItems =
                    case shouldAcc of
                        False ->
                            let
                                reindexedItemsArray =
                                    reindexedItems
                                        |> Array.fromList

                                maybeDraggedItem =
                                    reindexedItemsArray
                                        |> Array.get droppedAtIndex |> Debug.log "dragged item"

                                maybeDroppedItem =
                                    oldItems
                                        |> Array.fromList
                                        |> Array.get droppedAtIndex |> Debug.log "dropped item"

                            in
                            case (maybeDraggedItem, maybeDroppedItem) of
                                (Just draggedItem, Just droppedItem) ->
--                                    let
--                                        updatedItem =
--                                            { draggedItem | group = droppedItem.group }
--
--                                    in
--                                        Array.set droppedAtIndex updatedItem reindexedItemsArray
--                                            |> Array.toList
                                    reindexedItems
                                _ ->
                                    reindexedItems
                        True ->
                            reindexedItems


            in
            ( { model | draggable = draggable, items = updatedItems }
            , system.commands model.draggable
            )


getDroppedAt : Maybe Int -> Fruit -> (Int, Bool) -> (Int, Bool)
getDroppedAt maybeDragId fruit (index, shouldAcc) =
    case shouldAcc of
        True ->
            if Just fruit.position == maybeDragId then
                (index, False)
            else
                (index + 1, True)
        False ->
            (index, shouldAcc)