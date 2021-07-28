module L1.Parser.AST exposing
    ( Element(..)
    , Element_(..)
    , Name(..)
    , VerbatimType(..)
    , argsAndBody
    , body
    , body_
    , filter_
    , getArgs
    , getText
    , getTextList
    , getTitle
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

import L1.Library.Utility
import L1.Parser.Error exposing (..)
import L1.Parser.Loc as Loc
import L1.Parser.MetaData as MetaData exposing (MetaData)
import Parser.Advanced as Parser


type Element
    = Text String MetaData
    | Element Name (List Element) MetaData
    | Verbatim VerbatimType String MetaData
    | Problem (List ParseError) String


type VerbatimType
    = Code
    | Math
    | Quoted


toStringList : Element -> List String
toStringList element =
    case element of
        Text str _ ->
            [ str ]

        Element _ elements _ ->
            List.map getText elements

        Verbatim _ s _ ->
            [ s ]

        Problem _ _ ->
            [ "problems" ]


stringContent : List Element -> String
stringContent elements =
    elements |> List.map toStringList |> List.concat |> String.join " "


toList : Element -> List Element
toList element =
    case element of
        Text str _ ->
            [ element ]

        Element _ body__ _ ->
            body__

        Verbatim _ str _ ->
            [ element ]

        Problem _ _ ->
            [ element ]


getText : Element -> String
getText element =
    case element of
        Text s _ ->
            s

        Element _ body__ _ ->
            List.map getText body__ |> String.join " "

        Verbatim _ str _ ->
            str

        _ ->
            ""


getArgs : Element -> List String
getArgs element =
    (case element of
        Text s _ ->
            [ s ]

        Element _ elements _ ->
            List.map getArgs elements |> List.concat

        Verbatim _ str _ ->
            [ str ]

        _ ->
            []
    )
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


getTextList : Element -> List String
getTextList element =
    getText element |> String.words


type alias ParseError =
    Parser.DeadEnd Context Problem


type Name
    = Name String
    | UndefinedName


{-| A simplified version of the AST for humans
-}
type Element_
    = Text_ String
    | Element_ Name (List Element_)
    | Verbatim_ VerbatimType String
    | Problem_ Problem String


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

        Problem _ _ ->
            Loc.dummy


simplify : Element -> Element_
simplify element =
    case element of
        Text str _ ->
            Text_ str

        Element name body__ _ ->
            Element_ name (List.map simplify body__)

        Verbatim name content _ ->
            Verbatim_ name content

        Problem p s ->
            Problem_ (List.head p |> Maybe.map .problem |> Maybe.withDefault NoError) s



-- UTILITIES


join : Element -> List Element -> Element
join el list =
    case el of
        Element name list_ meta ->
            Element name (list_ ++ list) meta

        _ ->
            el


join2 : Element -> List Element -> Element
join2 el list =
    case el of
        Element name list_ meta ->
            Element name (list_ ++ list) meta

        _ ->
            el


body : Element -> List Element
body element =
    case element of
        Element _ list _ ->
            list

        _ ->
            []


body_ : Element_ -> List Element_
body_ element =
    case element of
        Element_ _ list ->
            list

        _ ->
            []


map : (String -> String) -> Element -> Element
map f element =
    case element of
        Text s meta ->
            Text (f s) meta

        Element name body__ meta ->
            Element name (List.map (map f) body__) meta

        _ ->
            element


indexedMap : (Int -> String -> String) -> List Element -> List Element
indexedMap f list =
    List.indexedMap (\k el -> map (f k) el) list


joinText : List Element -> String
joinText elements =
    List.map getText elements |> String.join " "


argsAndBody : Int -> List Element -> ( List String, List Element )
argsAndBody n elements =
    case elements of
        [] ->
            ( [], [] )

        first :: rest ->
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



--- HELPERS


getName : Element -> Maybe String
getName e =
    case e of
        Element (Name str) _ _ ->
            Just str

        _ ->
            Nothing


{-|

    pl "[i foo][b bar]" |> filter\_ "i" |> List.map L1.Parser.AST.simplify
    [ Element_ (Name "i") (EList_ [ Text_ "foo" ]) ]

-}
filter_ : String -> List Element -> List Element
filter_ name elements =
    List.filter (\e -> getName e == Just name) elements


firstLine : List String -> Maybe String
firstLine lines =
    case List.head lines of
        Just line ->
            if String.length (String.trim line) > 0 then
                Just line

            else
                firstLine (List.drop 1 lines)

        Nothing ->
            Nothing


getTitle : String -> String
getTitle str =
    case firstLine (String.lines str) of
        Nothing ->
            "Untitled"

        Just line ->
            if L1.Library.Utility.characterAt 0 line == "#" then
                String.dropLeft 2 line

            else
                "Untitled"
