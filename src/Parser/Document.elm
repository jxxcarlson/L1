module Parser.Document exposing (..)

import Parser.AST exposing (Element)
import Parser.Driver


type alias Document =
    String


parse : Int -> Document -> List (List Element)
parse generation doc =
    doc
        |> split
        |> List.map (Parser.Driver.parse generation)


split : Document -> List String
split doc =
    String.split "\n\n" doc
        |> List.filter (\s -> s /= "")
