module L1.Parser.MetaData exposing (MetaData, dummy, makeId)

import L1.Parser.Loc as Loc


type alias MetaData =
    { position : Loc.Position, generation : Int, id : String }


makeId : Int -> Int -> String
makeId generation count =
    String.fromInt generation ++ "-" ++ String.fromInt count


dummy =
    { position = { start = 0, end = 0 }, generation = 0, id = "bozo-0-0" }
