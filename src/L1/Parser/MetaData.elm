module L1.Parser.MetaData exposing (MetaData, dummy, makeId)

import L1.Parser.Loc as Loc


type alias MetaData =
    { position : Loc.StringPosition, generation : Int, location : Loc.ChunkLocation, id : String }


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
    { position = { start = 0, end = 0 }, generation = 0, location = { chunkIndex = -1, firstLine = -1 }, id = "dummy" }
