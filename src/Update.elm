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

                maybeDragId = system.draggedIndex draggable

                reindexedItems =
                    items
                        |> List.indexedMap (\index item -> { item | position = index })

                (droppedAtIndex, shouldAcc) =
                    List.foldl (getDroppedAt maybeDragId) (0, True) items

                updatedItems =
                    case shouldAcc of
                        False ->
                            let
                                blah = Debug.log "droppedAt" droppedAtIndex

                                maybeDraggedItem =
                                    reindexedItems
                                        |> List.foldl (\scu acc -> if scu.position == droppedAtIndex then scu :: acc else acc) []
                                        |> List.head
                                        |> Debug.log "dragged"

                                maybeDroppedItem =
                                    items
                                        |> List.foldl (\scu acc -> if scu.position == droppedAtIndex then scu :: acc else acc) []
                                        |> List.head
                                        |> Debug.log "dropped"

                            in
                            case (maybeDraggedItem, maybeDroppedItem) of
                                (Just draggedItem, Just droppedItem) ->
                                    let
                                        updatedItem =
                                            { draggedItem | group = droppedItem.group }

                                    in
                                    reindexedItems
                                        |> Array.fromList
                                        |> Array.set droppedAtIndex updatedItem
                                        |> Array.toList

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