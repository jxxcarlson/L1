module L1.Utility exposing (makeText)

import L1.AST exposing (Element(..))
import L1.MetaData as MetaData


makeText : String -> Element
makeText str =
    Text str MetaData.dummy
