module Parser.Utility exposing (makeText)

import Parser.AST exposing (Element(..))
import Parser.MetaData as MetaData


makeText : String -> Element
makeText str =
    Text str MetaData.dummy
