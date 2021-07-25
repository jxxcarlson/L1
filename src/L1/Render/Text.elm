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
                "$" ++ print body ++ "$"

            else
                "[" ++ name ++ " " ++ print body ++ "]"

        Verbatim _ content _ ->
            content

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

        Verbatim_ _ content ->
            content

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
