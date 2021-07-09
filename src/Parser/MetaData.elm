module Parser.MetaData exposing (MetaData, dummy, meta)

import Parser.Loc as Loc


type alias MetaData =
    { position : Loc.Position, generation : Int }


meta generation start end =
    { position = { start = start, end = end }, generation = generation }


dummy =
    { position = { start = 0, end = 0 }, generation = 0 }
