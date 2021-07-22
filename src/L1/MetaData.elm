module L1.MetaData exposing (MetaData, dummy)

import L1.Loc as Loc


type alias MetaData =
    { position : Loc.Position, generation : Int }


dummy =
    { position = { start = 0, end = 0 }, generation = 0 }
