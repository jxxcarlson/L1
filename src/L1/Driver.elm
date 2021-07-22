module L1.Driver exposing (parse, parseLoop, pl, pl_)

import L1.AST as AST exposing (Element(..))
import L1.Error exposing (Context(..), Problem(..))
import L1.Loop as Loop
import L1.Parser as Parser
import L1.TextCursor exposing (TextCursor)
import Parser.Advanced as PA


parseLoop : (String -> Element) -> Int -> String -> TextCursor
parseLoop parse_ generation str =
    Loop.parseLoop parse_ generation str


parse : (String -> Element) -> Int -> String -> List AST.Element
parse parse_ generation str =
    str
        |> parseLoop parse_ generation
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
