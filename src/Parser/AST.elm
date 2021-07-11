module Parser.AST exposing
    ( Element(..)
    , Element_(..)
    , Name(..)
    , body
    , body_
    , getText
    , join
    , joinText
    , length
    , map
    , position
    , simplify
    , toList
    , toStringList
    )

import Parser.Advanced as Parser
import Parser.Error exposing (..)
import Parser.Loc as Loc
import Parser.MetaData exposing (MetaData)


type Element
    = Text String MetaData
    | Element Name Element MetaData
    | EList (List Element) MetaData
    | Problem (List ParseError) String
    | StackError Int Int String String --- errorTextStart errorTextEnd message errorText
    | Empty


toStringList : Element -> List String
toStringList element =
    case element of
        Text str _ ->
            [ str ]

        Element _ body__ _ ->
            toStringList body__

        EList elements _ ->
            List.map getText elements

        Problem _ _ ->
            [ "problems" ]

        StackError _ _ a b ->
            [ a, b ]

        Empty ->
            [ "empty" ]


toList : Element -> List Element
toList element =
    case element of
        Text str _ ->
            [ element ]

        Element _ body__ _ ->
            toList body__

        EList elements _ ->
            elements

        Problem _ _ ->
            [ element ]

        StackError _ _ a b ->
            [ element ]

        Empty ->
            [ element ]


getText : Element -> String
getText element =
    case element of
        Text s _ ->
            s

        _ ->
            ""


type alias ParseError =
    Parser.DeadEnd Context Problem


type Name
    = Name String
    | Undefined


{-| A simplified version of the AST for humans
-}
type Element_
    = Text_ String
    | Element_ Name Element_
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
        Text _ meta ->
            meta.position

        Element _ _ meta ->
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
        Text str _ ->
            Text_ str

        Element name el _ ->
            Element_ name (simplify el)

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
        Element name (EList list1 _) meta ->
            Element name (EList (list1 ++ list) Parser.MetaData.dummy) meta

        _ ->
            el


body : Element -> List Element
body element =
    case element of
        Element _ (EList list _) _ ->
            list

        _ ->
            []


body_ : Element_ -> List Element_
body_ element =
    case element of
        Element_ _ (EList_ list) ->
            list

        _ ->
            []


map : (String -> String) -> Element -> Element
map f element =
    case element of
        Text s meta ->
            Text (f s) meta

        _ ->
            element


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "
