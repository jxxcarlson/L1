module Parser.Driver exposing (parse, parseLoop, pl)

import Parser.AST as AST exposing (Element(..))
import Parser.Advanced as PA
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Loop as Loop
import Parser.Parser as Parser
import Parser.TextCursor exposing (TextCursor)


parseLoop : (String -> Element) -> Int -> String -> TextCursor
parseLoop parse_ generation str =
    Loop.parseLoop parse_ generation str


parse : (String -> Element) -> Int -> String -> List Element
parse parse_ generation str =
    str
        |> parseLoop parse_ generation
        |> .complete


{-| Used for testing
-}
pl : String -> List AST.Element_
pl str =
    let
        tc =
            parseLoop (Parser.parse 0) 0 str
    in
    tc |> .complete |> List.map AST.simplify



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem
