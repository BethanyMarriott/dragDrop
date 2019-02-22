module Model exposing (Model, initialModel)


-- MODEL


import Config exposing (Fruit)


import DnDList

type alias Model =
    { draggable : DnDList.Draggable
    , items : List Fruit
    }


initialModel : Model
initialModel =
    { draggable = Config.system.draggable
    , items = Config.data
    }