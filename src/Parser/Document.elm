module Parser.Document exposing (..)

import Parser.AST exposing (Element)
import Parser.Driver
import Parser.Parser


type alias Document =
    String


parse : Int -> Document -> List (List Element)
parse generation doc =
    let
        p : String -> List Element
        p =
            Parser.Driver.parse (Parser.Parser.parse generation) generation
    in
    doc
        |> split
        |> List.map p


split : Document -> List String
split doc =
    String.split "\n\n" doc
        |> List.filter (\s -> s /= "")
