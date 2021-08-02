module L1.Parser.MetaData exposing (MetaData, dummy, getLabel, makeId)

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


getLabel : MetaData -> String
getLabel meta =
    case meta.info of
        Nothing ->
            ""

        Just info ->
            info.label


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
