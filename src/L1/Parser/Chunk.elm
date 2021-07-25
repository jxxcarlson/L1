module L1.Parser.Chunk exposing (parse, parseLoop, pl, pl_)

import L1.Parser.AST as AST exposing (Element(..))
import L1.Parser.Error exposing (Context(..), Problem(..))
import L1.Parser.Loop as Loop
import L1.Parser.Parser as Parser
import L1.Parser.TextCursor exposing (TextCursor)
import Parser.Advanced as PA


parseLoop : (String -> Element) -> Int -> String -> TextCursor
parseLoop parser generation str =
    Loop.parseLoop parser generation str


parse : (String -> Element) -> Int -> String -> List AST.Element
parse parser generation str =
    str
        |> parseLoop parser generation
        |> .complete


pl : String -> List AST.Element
pl str =
    let
        tc =
            parseLoop (Parser.parse 0) 0 str
    in
    tc |> .complete


{-| Used for testing
-}
pl_ : String -> List AST.Element_
pl_ str =
    let
        tc =
            parseLoop (Parser.parse 0) 0 str
    in
    tc |> .complete |> List.map AST.simplify



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem
