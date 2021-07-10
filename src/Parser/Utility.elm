module Parser.Utility exposing (getText, joinText, makeRaw, mapRaw)

import Parser.AST exposing (Element(..))
import Parser.MetaData as MetaData


getText : Element -> String
getText element =
    case element of
        Text s _ ->
            s

        _ ->
            ""


makeRaw : String -> Element
makeRaw str =
    Text str MetaData.dummy


mapRaw : (String -> String) -> Element -> Element
mapRaw f element =
    case element of
        Text s meta ->
            Text (f s) meta

        _ ->
            element


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "
