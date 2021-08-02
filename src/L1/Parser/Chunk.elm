module L1.Parser.Chunk exposing (parse, parseLoop, pl, pl_, pl__)

import L1.Parser.AST as AST exposing (Element(..))
import L1.Parser.Error exposing (Context(..), Problem(..))
import L1.Parser.Loc as Loc
import L1.Parser.Loop as Loop
import L1.Parser.Parser as Parser
import L1.Parser.TextCursor exposing (Accumulator, TextCursor, emptyAccumulator)
import Parser.Advanced as PA


parseLoop : (Int -> Loc.ChunkLocation -> Int -> String -> Element) -> Accumulator -> Int -> Loc.ChunkLocation -> String -> TextCursor
parseLoop parser accumulator generation chunkLocation str =
    Loop.parseLoop parser accumulator generation chunkLocation str


parse : (Int -> Loc.ChunkLocation -> Int -> String -> Element) -> Accumulator -> Int -> Loc.ChunkLocation -> String -> List AST.Element
parse parser accumulator generation chunkLocation str =
    str
        |> parseLoop parser accumulator generation chunkLocation
        |> .complete


pl : Int -> String -> List AST.Element
pl scanPoint str =
    let
        tc =
            parseLoop Parser.parse emptyAccumulator 3 { chunkIndex = 3, firstLine = 0 } str
    in
    tc |> .complete


{-| Used for testing
-}
pl_ : String -> List AST.Element_
pl_ str =
    let
        tc =
            parseLoop Parser.parse emptyAccumulator 1 { chunkIndex = 0, firstLine = 0 } str
    in
    tc |> .complete |> List.map AST.simplify


{-| Used for testing
-}
pl__ : String -> List AST.Element__
pl__ str =
    let
        tc =
            parseLoop Parser.parse emptyAccumulator 3 { chunkIndex = 3, firstLine = 0 } str
    in
    tc |> .complete |> List.map AST.simplify_



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem
