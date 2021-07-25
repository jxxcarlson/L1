module L1.Render.Markdown exposing (testData, transform, transformDocument, transformList)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import L1.Parser.AST as AST exposing (Element(..), Element_(..), Name(..), VerbatimType(..))
import L1.Parser.Document
import L1.Parser.Error exposing (Context(..), Problem(..))
import Parser.Advanced


codeMark =
    "`"


italicMark =
    "*"


boldMark =
    "**"


strikeMark =
    "~~"


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


type alias RenderArgs =
    { width : Int
    }


type alias FRender =
    RenderArgs -> String -> List String -> Element -> String


type alias RenderElementDict =
    Dict String FRender


testData =
    """### Text Cursor

To explain the  scanning/cursor idea, let us consider the two snippets

(a) `This [i is] a [b real] test!`
"""


renderElementDict : RenderElementDict
renderElementDict =
    Dict.fromList
        [ ( "i", italic )
        , ( "b", bold )
        , ( "strike", strike )
        , ( "code", code )
        , ( "quoted", quoted )
        , ( "math2", math )
        , ( "m", math )
        , ( "mathblock", mathblock )
        , ( "mb", mathblock )
        , ( "link", link )
        , ( "image", image )
        , ( "heading", heading )
        , ( "item", item )
        ]


transformDocument : String -> String
transformDocument doc =
    doc
        |> L1.Parser.Document.parse 0
        |> List.map (transformList { width = 600 })
        |> String.join "\n\n"


transformList : RenderArgs -> List Element -> String
transformList renderArgs list =
    List.map (transform renderArgs) list |> String.join " "


transform : RenderArgs -> Element -> String
transform renderArgs element =
    case element of
        Text str _ ->
            str

        Element (Name name) body _ ->
            renderWithDictionary renderArgs name [] body

        Verbatim verbatimType content _ ->
            renderVerbatim verbatimType content

        Element Undefined body _ ->
            "Undefined element"

        EList elements _ ->
            List.map (AST.map (\s -> " " ++ s) >> transform renderArgs) elements |> String.join ""

        Problem _ str ->
            "PROBLEM: " ++ str

        StackError _ _ message errorText ->
            errorText ++ " " ++ message

        Empty ->
            "EMPTY"


renderVerbatim verbatimType content =
    case verbatimType of
        Math ->
            "$" ++ content ++ "$"

        Code ->
            "`" ++ content ++ "`"

        Quoted ->
            content


renderWithDictionary renderArgs name args body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name args body

        Nothing ->
            "[" ++ name ++ " " ++ transform renderArgs body ++ "]"



-- TEXT STYLE


italic : FRender
italic renderArgs _ _ body =
    italicMark ++ String.trim (transform renderArgs body) ++ italicMark


bold : FRender
bold renderArgs _ _ body =
    boldMark ++ String.trim (transform renderArgs body) ++ boldMark


strike : FRender
strike renderArgs _ _ body =
    strikeMark ++ String.trim (transform renderArgs body) ++ strikeMark


quoted : FRender
quoted renderArgs _ _ body =
    transform renderArgs body


code : FRender
code renderArgs _ _ body =
    codeMark ++ String.trim (transform renderArgs body) ++ codeMark


link : FRender
link renderArgs name args body =
    let
        bodyStrings : List String
        bodyStrings =
            -- getText body |> Maybe.withDefault "missing url"
            case body of
                EList elements _ ->
                    List.map AST.getText elements

                _ ->
                    [ "missing", "stuff" ]

        ( label, url ) =
            case bodyStrings of
                label_ :: url_ :: rest ->
                    ( label_, url_ )

                url_ :: [] ->
                    ( url_, url_ )

                [] ->
                    ( "no label", "https://nowhere.com" )
    in
    "[" ++ label ++ "](" ++ url ++ ")"


heading : FRender
heading renderArgs name args body =
    let
        text =
            getText body |> Maybe.withDefault "TITLE"

        level =
            text
                |> String.words
                |> List.filter (\s -> String.left 1 s == "#")
                |> List.head
                |> Maybe.withDefault ""
                |> String.length

        prefix =
            String.repeat level "#"

        title =
            String.replace prefix "" text
    in
    "#" ++ prefix ++ " " ++ title


item : FRender
item renderArgs name args body =
    "-  " ++ transform renderArgs body


image : FRender
image renderArgs name _ body =
    let
        url =
            getTextList body |> List.reverse |> List.head |> Maybe.withDefault "no-image"
    in
    "![ IMAGE ](" ++ url ++ ")"



-- Math


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


math : FRender
math renderArgs name args body =
    "$" ++ getText2 body ++ "$"


mathblock : FRender
mathblock rendArgs name args body =
    "$$" ++ getText2 body ++ "$$"



-- HELPERS


toInt : String -> Int
toInt x =
    x |> String.trim |> String.toInt |> Maybe.withDefault 0


getInt : Element -> Int
getInt e =
    e |> getText2 |> toInt


getText2 : Element -> String
getText2 element =
    case element of
        Text s _ ->
            s

        EList list _ ->
            List.map getText2 list |> String.join ""

        _ ->
            ""


getTextList : Element -> List String
getTextList element =
    case element of
        Text s _ ->
            s :: []

        EList list _ ->
            List.map getText2 list

        _ ->
            []


getText : Element -> Maybe String
getText element =
    case element of
        EList [ Text content _ ] _ ->
            Just content

        _ ->
            Nothing
