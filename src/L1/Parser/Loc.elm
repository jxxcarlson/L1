module L1.Parser.Loc exposing (ChunkLocation, StringPosition, dummyPosition, positionOfList)

{-| Used to identify a piece of text in the source
-}


type alias StringPosition =
    { start : Int, end : Int }


type alias ChunkLocation =
    { index : Int, firstLine : Int }


dummyPosition =
    { start = -1, end = -1 }


positionOfList : List StringPosition -> StringPosition
positionOfList positions =
    let
        sorted =
            List.sortBy (\pos -> pos.start) positions

        first =
            List.head sorted |> Maybe.map .start |> Maybe.withDefault 0

        last =
            List.head (List.reverse sorted) |> Maybe.map .end |> Maybe.withDefault 0
    in
    { start = first, end = last }
