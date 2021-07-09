module Parser.AST exposing (Element(..), Element_(..), Name(..), body, body_, join, length, position, simplify)

import Parser.Advanced as Parser
import Parser.Error exposing (..)
import Parser.Loc as Loc
import Parser.MetaData exposing (MetaData)


type Element
    = Raw String MetaData
    | Element Name (List String) Element MetaData
    | EList (List Element) MetaData
    | Problem (List ParseError) String
    | StackError Int Int String String --- errorTextStart errorTextEnd message errorText
    | Empty


type alias ParseError =
    Parser.DeadEnd Context Problem


type Name
    = Name String
    | Undefined


{-| A simplified version of the AST for humans
-}
type Element_
    = Raw_ String
    | Element_ Name (List String) Element_
    | EList_ (List Element_)
    | Problem_ Problem String
    | StackError_ Int Int String String -- errorTextStart errorTextEnd message errorText
    | Incomplete_


length : Element -> Int
length element =
    let
        pos =
            position element
    in
    pos.end - pos.start


position : Element -> Loc.Position
position element =
    case element of
        Raw _ meta ->
            meta.position

        Element _ _ _ meta ->
            meta.position

        EList _ meta ->
            meta.position

        Problem _ _ ->
            Loc.dummy

        StackError _ _ _ _ ->
            Loc.dummy

        Empty ->
            Loc.dummy


simplify : Element -> Element_
simplify element =
    case element of
        Raw str _ ->
            Raw_ str

        Element name strList el _ ->
            Element_ name strList (simplify el)

        EList elementList _ ->
            EList_ (List.map simplify elementList)

        Problem p s ->
            Problem_ (List.head p |> Maybe.map .problem |> Maybe.withDefault NoError) s

        StackError startError endError message errorText ->
            StackError_ startError endError message errorText

        Empty ->
            Incomplete_



-- UTILITIES


join : Element -> List Element -> Element
join el list =
    case el of
        Element name args (EList list1 _) meta ->
            Element name args (EList (list1 ++ list) Parser.MetaData.dummy) meta

        _ ->
            el


body : Element -> List Element
body element =
    case element of
        Element _ _ (EList list _) _ ->
            list

        _ ->
            []


body_ : Element_ -> List Element_
body_ element =
    case element of
        Element_ _ _ (EList_ list) ->
            list

        _ ->
            []
