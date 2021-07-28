module L1.Render.Text exposing (print, printList_, print_)

import L1.Parser.AST exposing (Element(..), Element_(..), Name(..))
import L1.Parser.Error exposing (Context(..), Problem(..))
import Parser.Advanced


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


print : Element -> String
print element =
    case element of
        Text str _ ->
            str

        Element (Name name) body _ ->
            if name == "math" then
                "$" ++ String.join " " (List.map print body) ++ "$"

            else
                "[" ++ name ++ " " ++ String.join " " (List.map print body) ++ "]"

        Verbatim _ content _ ->
            content

        Element Undefined body _ ->
            "[" ++ "undefined" ++ String.join " " (List.map print body) ++ "]"

        Problem _ str ->
            "PROBLEM: " ++ str


print_ : Element_ -> String
print_ element =
    case element of
        Text_ str ->
            str

        Element_ (Name name) body ->
            "[" ++ name ++ " " ++ String.join " " (List.map print_ body) ++ "]"

        Element_ Undefined body ->
            "[" ++ "undefined" ++ String.join " " (List.map print_ body) ++ "]"

        Verbatim_ _ content ->
            content

        Problem_ _ str ->
            "PROBLEM: " ++ str


printList_ : List Element_ -> String
printList_ elements =
    String.join " " (List.map print_ elements)
