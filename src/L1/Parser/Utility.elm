module L1.Parser.Utility exposing (makeText)

import L1.Parser.AST exposing (Element(..))
import L1.Parser.MetaData as MetaData


makeText : String -> Element
makeText str =
    Text str MetaData.dummy
