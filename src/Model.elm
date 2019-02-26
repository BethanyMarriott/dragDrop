module Model exposing (Model, initialModel, Items(..))


-- MODEL


import Config exposing (Fruit)


import DnDList

type alias Model =
    { draggable : DnDList.Draggable
    , items : Items
    }

type Items
    = Dragging String (List Fruit) (List Fruit)
    | NotDragging (List Fruit)

initialModel : Model
initialModel =
    { draggable = Config.system.draggable
    , items = NotDragging Config.data
    }