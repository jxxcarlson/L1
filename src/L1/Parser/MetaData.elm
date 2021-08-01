module L1.Parser.MetaData exposing (MetaData, dummy, makeId)

import L1.Parser.Loc as Loc


type alias MetaData =
    { -- location in source text
      position : Loc.StringPosition
    , location : Loc.ChunkLocation

    --
    , generation : Int
    , id : String

    --
    , info : Maybe Info
    }


type alias Info =
    { label : String }


makeId : Int -> Loc.ChunkLocation -> Loc.StringPosition -> String
makeId generation chunkLocation stringLocation =
    String.fromInt generation
        ++ "."
        ++ String.fromInt chunkLocation.chunkIndex
        ++ "."
        ++ String.fromInt chunkLocation.firstLine
        ++ "."
        ++ String.fromInt stringLocation.start
        ++ "."
        ++ String.fromInt stringLocation.end


dummy =
    { position = { start = 0, end = 0 }
    , generation = 0
    , location = { chunkIndex = -1, firstLine = -1 }
    , id = "dummy"
    , info = Nothing
    }
