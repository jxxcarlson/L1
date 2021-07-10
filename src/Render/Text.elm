module Render.Text exposing (print, printList_, print_)

import Parser.AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Error exposing (Context(..), Problem(..))


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


print : Element -> String
print element =
    case element of
        Text str _ ->
            str

        Element (Name name) body _ ->
            if name == "math" then
                "$" ++ print body ++ "$"

            else
                "[" ++ name ++ " " ++ print body ++ "]"

        Element Undefined body _ ->
            "[" ++ "undefined" ++ print body ++ "]"

        EList elements _ ->
            String.join " " (List.map print elements)

        Problem _ str ->
            "PROBLEM: " ++ str

        StackError _ _ message errorText ->
            message ++ ":  " ++ errorText

        Empty ->
            "EMPTY"


print_ : Element_ -> String
print_ element =
    case element of
        Text_ str ->
            str

        Element_ (Name name) body ->
            "[" ++ name ++ " " ++ print_ body ++ "]"

        Element_ Undefined body ->
            "[" ++ "undefined" ++ print_ body ++ "]"

        EList_ elements ->
            String.join " " (List.map print_ elements)

        Problem_ _ str ->
            "PROBLEM: " ++ str

        StackError_ _ _ message errorText ->
            "((" ++ message ++ ":  " ++ errorText ++ "))"

        Incomplete_ ->
            "EMPTY"


printList_ : List Element_ -> String
printList_ elements =
    String.join " " (List.map print_ elements)



--rt : String -> Bool
--rt str =
--    (Parser.Driver.pl str |> printList_) == str
--
--
--rt_ : String -> Bool
--rt_ str =
--    (Parser.Driver.pl str |> printList_ |> normalize) == normalize str
