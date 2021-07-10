module Parser.Utility exposing (getText, joinText)

import Parser.AST exposing (Element(..))


getText : Element -> String
getText element =
    case element of
        Raw s _ ->
            s

        _ ->
            ""


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "
