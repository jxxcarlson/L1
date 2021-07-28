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
    RenderArgs -> String -> List Element -> String


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
        , ( "heading1", heading1 )
        , ( "heading2", heading2 )
        , ( "heading3", heading3 )
        , ( "heading4", heading4 )
        , ( "codeblock", codeblock )
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
            renderWithDictionary renderArgs name (renderList renderArgs body)

        Verbatim verbatimType content _ ->
            renderVerbatim verbatimType content

        Element Undefined body _ ->
            "Undefined element"

        Problem _ str ->
            "PROBLEM: " ++ str


renderList renderArgs elements =
    List.map (AST.map (\s -> " " ++ s) >> transform renderArgs) elements |> String.join ""


renderVerbatim verbatimType content =
    case verbatimType of
        Math ->
            "$" ++ content ++ "$"

        Code ->
            "`" ++ content ++ "`"

        Quoted ->
            content


renderWithDictionary renderArgs name body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name body

        Nothing ->
            "[" ++ name ++ " " ++ transformList renderArgs body ++ "]"



-- TEXT STYLE


italic : FRender
italic renderArgs _ body =
    italicMark ++ String.trim (transformList renderArgs body) ++ italicMark


bold : FRender
bold renderArgs _ body =
    boldMark ++ String.trim (transformList renderArgs body) ++ boldMark


strike : FRender
strike renderArgs _ body =
    strikeMark ++ String.trim (transformList renderArgs body) ++ strikeMark


quoted : FRender
quoted renderArgs _ body =
    transformList renderArgs body


code : FRender
code renderArgs _ body =
    codeMark ++ String.trim (transformList renderArgs body) ++ codeMark


link : FRender
link renderArgs name body =
    let
        bodyStrings : List String
        bodyStrings =
            List.map AST.getText body

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
heading renderArgs name body =
    let
        text =
            getTextList body |> String.join " "

        -- |> Maybe.withDefault "TITLE"
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
item renderArgs name body =
    "-  " ++ transformList renderArgs body


heading1 : FRender
heading1 renderArgs name body =
    "# " ++ transformList renderArgs body


heading2 : FRender
heading2 renderArgs name body =
    "## " ++ transformList renderArgs body


heading3 : FRender
heading3 renderArgs name body =
    "### " ++ transformList renderArgs body


heading4 : FRender
heading4 renderArgs name body =
    "#### " ++ transformList renderArgs body


image : FRender
image renderArgs name body =
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
math renderArgs name body =
    "$" ++ (getTextList body |> String.join " ") ++ "$"


mathblock : FRender
mathblock rendArgs name body =
    "$$" ++ (getTextList body |> String.join " ") ++ "$$"


codeblock : FRender
codeblock rendArgs name body =
    "```\n" ++ (getTextList body |> String.join " ") ++ "\n```"



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

        _ ->
            ""


getTextList : List Element -> List String
getTextList elements =
    List.map getText2 elements


getText : Element -> Maybe String
getText element =
    case element of
        Text content _ ->
            Just content

        _ ->
            Nothing
