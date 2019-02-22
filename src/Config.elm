module Config exposing (..)


-- DATA


import DnDList
import Msg exposing (Msg)


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
    , (Fruit "id-Cherries" 2 0 "Oranges")
    , (Fruit "id-Dates" 3 1 "Pears")
    , (Fruit "id-Cherries" 4 1 "Cherries")
    , (Fruit "id-Grapes" 5 1 "Grapes")
    ]



-- SYSTEM


config : DnDList.Config Msg
config =
    { message = Msg.DndMsg
    , movement = DnDList.Free
    }


system : DnDList.System Msg Fruit
system =
    DnDList.create config