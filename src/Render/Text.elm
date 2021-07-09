module Render.Text exposing (normalize, print, printList_, print_, rt, rt_)

import Parser.AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Driver
import Parser.Error exposing (Context(..), Problem(..))
import Parser.MetaData exposing (MetaData)
import Regex


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem



-- type Element
--     = Raw String MetaData
--     | Element Name (List String) Element MetaData
--     | EList (List Element) MetaData
--     | Problem (List ParseError) String
--     | Empty
-- type Element_
--     = Raw_ String
--     | Element_ Name (List String) Element_
--     | EList_ (List Element_)
--     | Problem_ Problem String
--     | Incomplete_


print : Element -> String
print element =
    case element of
        Raw str _ ->
            str

        Element (Name name) _ body _ ->
            "[" ++ name ++ print body ++ "]"

        Element Undefined _ body _ ->
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
        Raw_ str ->
            str

        Element_ (Name name) _ body ->
            "[" ++ name ++ " " ++ print_ body ++ "]"

        Element_ Undefined _ body ->
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


rt : String -> Bool
rt str =
    (Parser.Driver.pl str |> printList_) == str


rt_ : String -> Bool
rt_ str =
    (Parser.Driver.pl str |> printList_ |> normalize) == normalize str


squeeze : String -> String
squeeze str =
    String.replace " " "" str


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


normalize : String -> String
normalize string =
    userReplace " +" (\_ -> " ") string
