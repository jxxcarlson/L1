module L1.Parser.AST exposing
    ( Element(..)
    , Element_(..)
    , Element__(..)
    , Name(..)
    , VerbatimType(..)
    , argsAndBody
    , body
    , body_
    , getArgs
    , getText
    , getTextList
    , getTextList2
    , getTitle
    , indexedMap
    , join
    , joinText
    , length
    , makeTOC
    , map
    , position
    , simplify
    , simplify_
    , stringContent
    , toList
    , toStringList
    )

import L1.Library.Utility
import L1.Parser.Error exposing (..)
import L1.Parser.Loc as Loc
import L1.Parser.MetaData as MetaData exposing (MetaData)
import Maybe.Extra
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


getTextList2 : Element -> List String
getTextList2 e =
    case e of
        Text s _ ->
            [ s ]

        Element _ body__ _ ->
            List.map getText body__

        Verbatim _ str _ ->
            [ str ]

        _ ->
            []


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
    = Text_ String String
    | Element_ Name (List Element_) String
    | Verbatim_ VerbatimType String String
    | Problem_ Problem String


type Element__
    = Text__ String
    | Element__ Name (List Element__)
    | Verbatim__ VerbatimType String
    | Problem__ Problem String


length : Element -> Int
length element =
    let
        pos =
            position element
    in
    pos.end - pos.start


position : Element -> Loc.StringPosition
position element =
    case element of
        Text _ meta ->
            meta.position

        Element _ _ meta ->
            meta.position

        Verbatim _ _ meta ->
            meta.position

        Problem _ _ ->
            Loc.dummyPosition


simplify : Element -> Element_
simplify element =
    case element of
        Text str meta ->
            Text_ str meta.id

        Element name body__ meta ->
            Element_ name (List.map simplify body__) meta.id

        Verbatim name content meta ->
            Verbatim_ name content meta.id

        Problem p s ->
            Problem_ (List.head p |> Maybe.map .problem |> Maybe.withDefault NoError) s


simplify_ : Element -> Element__
simplify_ element =
    case element of
        Text str _ ->
            Text__ str

        Element name body__ _ ->
            Element__ name (List.map simplify_ body__)

        Verbatim name content _ ->
            Verbatim__ name content

        Problem p s ->
            Problem__ (List.head p |> Maybe.map .problem |> Maybe.withDefault NoError) s



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
        Element_ _ list _ ->
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
filterOnNames : List String -> List Element -> List ( String, Element )
filterOnNames names elements =
    List.filter (\( n, _ ) -> List.member n names)
        (List.map (\e -> ( getName e |> Maybe.withDefault "@#@", e )) elements)


getHeadings : List (List Element) -> List ( String, String )
getHeadings list =
    list
        |> List.concat
        |> filterOnNames [ "heading1", "heading2", "heading3" ]
        |> List.map (\( s, e ) -> ( s, getText e ))


{-| Make table of contents from the AST
-}
makeTOC : List (List Element) -> Element
makeTOC ast =
    ast
        |> getHeadings
        |> List.map makeTocItem
        |> enclose "toc"


enclose : String -> List Element -> Element
enclose name elements =
    Element (Name name) elements MetaData.dummy


makeTocItem : ( String, String ) -> Element
makeTocItem ( headingType, name ) =
    case headingType of
        "heading1" ->
            Element (Name "tocItem") [ Text "1" MetaData.dummy, Text name MetaData.dummy ] MetaData.dummy

        "heading2" ->
            Element (Name "tocItem") [ Text "2" MetaData.dummy, Text name MetaData.dummy ] MetaData.dummy

        "heading3" ->
            Element (Name "tocItem") [ Text "3" MetaData.dummy, Text name MetaData.dummy ] MetaData.dummy

        "heading4" ->
            Element (Name "tocItem") [ Text "4" MetaData.dummy, Text name MetaData.dummy ] MetaData.dummy

        _ ->
            Element (Name "tocItem") [ Text "5" MetaData.dummy, Text name MetaData.dummy ] MetaData.dummy


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
