module Parser.Utility exposing (getText, joinText, makeText, mapElement)

import Parser.AST exposing (Element(..))
import Parser.MetaData as MetaData


getText : Element -> String
getText element =
    case element of
        Text s _ ->
            s

        _ ->
            ""


makeText : String -> Element
makeText str =
    Text str MetaData.dummy


mapElement : (String -> String) -> Element -> Element
mapElement f element =
    case element of
        Text s meta ->
            Text (f s) meta

        _ ->
            element


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "
