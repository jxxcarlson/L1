module L1.AST exposing
    ( Element(..)
    , Element_(..)
    , Name(..)
    , VerbatimType(..)
    , argsAndBody
    , body
    , body_
    , getText
    , getTextList
    , indexedMap
    , join
    , joinText
    , length
    , map
    , position
    , simplify
    , stringContent
    , toList
    , toStringList
    )

import L1.Error exposing (..)
import L1.Loc as Loc
import L1.MetaData as MetaData exposing (MetaData)
import Parser.Advanced as Parser


type Element
    = Text String MetaData
    | Element Name Element MetaData
    | Verbatim VerbatimType String MetaData
    | EList (List Element) MetaData
    | Problem (List ParseError) String
    | StackError Int Int String String --- errorTextStart errorTextEnd message errorText
    | Empty


type VerbatimType
    = Code
    | Math
    | Quoted


toStringList : Element -> List String
toStringList element =
    case element of
        Text str _ ->
            [ str ]

        Element _ body__ _ ->
            toStringList body__

        Verbatim _ s _ ->
            [ s ]

        EList elements _ ->
            List.map getText elements

        Problem _ _ ->
            [ "problems" ]

        StackError _ _ a b ->
            [ a, b ]

        Empty ->
            [ "empty" ]


stringContent : Element -> String
stringContent element =
    element |> toStringList |> String.join " "


toList : Element -> List Element
toList element =
    case element of
        Text str _ ->
            [ element ]

        Element _ body__ _ ->
            toList body__

        Verbatim _ str _ ->
            [ element ]

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

        Element _ body__ _ ->
            getText body__

        EList list _ ->
            List.map getText list |> String.join " "

        _ ->
            ""


getTextList : Element -> List String
getTextList element =
    getText element |> String.words


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
    | Verbatim_ VerbatimType String
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

        Verbatim _ _ meta ->
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

        Verbatim name content _ ->
            Verbatim_ name content

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
            Element name (EList (list1 ++ list) MetaData.dummy) meta

        _ ->
            el


join2 : Element -> List Element -> Element
join2 el list =
    case el of
        Element name (EList list1 _) meta ->
            Element name (EList (list1 ++ list) MetaData.dummy) meta

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

        Element name body__ meta ->
            Element name (map f body__) meta

        EList items meta ->
            EList (List.map (map f) items) meta

        _ ->
            element


indexedMap : (Int -> String -> String) -> List Element -> List Element
indexedMap f list =
    List.indexedMap (\k el -> map (f k) el) list


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "


argsAndBody : Int -> Element -> ( List String, List Element )
argsAndBody n element =
    case element of
        Element name body__ meta ->
            case body__ of
                EList [] _ ->
                    ( [], [] )

                EList (first :: rest) _ ->
                    let
                        args =
                            getText first |> String.words

                        realArgs =
                            List.take n args

                        lastWords =
                            List.drop n args |> String.join " "

                        elt =
                            Text lastWords MetaData.dummy
                    in
                    ( realArgs, elt :: rest )

                _ ->
                    ( [], [] )

        EList (first :: rest) _ ->
            let
                args =
                    getText first |> String.words

                realArgs =
                    List.take n args

                lastWords =
                    List.drop n args |> String.join " "

                elt =
                    Text lastWords MetaData.dummy
            in
            ( realArgs, elt :: rest )

        _ ->
            ( [], [] )
