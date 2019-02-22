module Msg exposing (Msg(..))


import DnDList


type Msg
    = NoOp
   | DndMsg DnDList.Msg