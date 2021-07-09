module Parser.Driver exposing (parse, parseLoop, pl)

import Parser.AST as AST exposing (Element(..))
import Parser.Advanced as PA
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Loop as Loop
import Parser.Parser as Parser
import Parser.TextCursor as TextCursor exposing (ErrorStatus(..), TextCursor)


{-| The value of Loop.Packet that we need here
-}
packet : Loop.Packet Element
packet =
    { parser = \str -> Parser.parse 0 str
    , getLength = AST.length
    , handleError = Nothing
    }


parseLoop : Int -> String -> TextCursor
parseLoop generation str =
    Loop.parseLoop packet generation str


parse : Int -> String -> List Element
parse generation str =
    str
        |> parseLoop generation
        |> .complete


{-| Used for testing
-}
pl : String -> List AST.Element_
pl str =
    let
        tc =
            parseLoop 0 str

        _ =
            Debug.log "FINAL OFFSET" tc.offset
    in
    tc |> .complete |> List.map AST.simplify



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem
