module L1.Render.Elm exposing (..)

{- (convertRGB, render, renderList) -}

import Dict exposing (Dict)
import Element as E exposing (column, el, fill, paddingEach, paragraph, px, rgb, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import L1.Library.Utility as Utility
import L1.Parser.AST as AST exposing (Element(..), Element_(..), Name(..), VerbatimType(..))
import L1.Parser.Error exposing (Context(..), Problem(..))
import L1.Parser.MetaData as MetaData
import L1.Parser.Utility
import Parser.Advanced


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


{-| The 'generation' field is used by 'mathText', which
is keyed node. The generation is updated on each
keystroke (or debounced keystroke). One has to do
this to inform the virtual DOM that the math
element has changed. Otherwise it will not respond
to edits.
-}
type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    }


type alias FRender msg =
    RenderArgs -> String -> List Element -> E.Element msg


type alias RenderElementDict msg =
    Dict String (FRender msg)


renderElementDict : RenderElementDict msg
renderElementDict =
    Dict.fromList
        [ ( "i", italic )
        , ( "b", bold )
        , ( "strike", strike )
        , ( "underline", underline )
        , ( "hide", hide )
        , ( "highlight", highlight )
        , ( "bluelight", bluelight )
        , ( "redlight", redlight )
        , ( "highlightRGB", highlight )
        , ( "fontRGB", fontRGB )
        , ( "red", red )
        , ( "error", error )
        , ( "blue", blue )
        , ( "violet", violet )
        , ( "gray", gray )
        , ( "code", code )
        , ( "quoted", quoted )
        , ( "math2", math2 )
        , ( "m", renderMath )
        , ( "mathblock", mathblock )
        , ( "codeblock", codeblock )
        , ( "indent", indent )
        , ( "mb", mathblock )
        , ( "link", link )
        , ( "image", image )
        , ( "heading1", heading1 )
        , ( "heading2", heading2 )
        , ( "heading3", heading3 )
        , ( "heading4", heading4 )
        , ( "item", item )
        ]


renderList : RenderArgs -> List Element -> List (E.Element msg)
renderList renderArgs list =
    List.map (render renderArgs) list


render : RenderArgs -> Element -> E.Element msg
render renderArgs element =
    case element of
        Text str _ ->
            E.el [] (text str)

        Element (Name name) body _ ->
            renderWithDictionary renderArgs name body

        Verbatim verbatimType str _ ->
            renderVerbatim renderArgs verbatimType str

        Element Undefined body _ ->
            E.el [] (text <| "Undefined element")

        Problem _ str ->
            -- el [] (text <| "PROBLEM: " ++ str)
            E.paragraph [] [ el [ Background.color lightRedColor ] (text <| "PROBLEM: "), el [ Background.color lightBlueColor ] (text <| str) ]


renderVerbatim : RenderArgs -> VerbatimType -> String -> E.Element msg
renderVerbatim renderArgs verbatimType content =
    case verbatimType of
        Math ->
            mathText renderArgs InlineMathMode content

        Code ->
            code1 renderArgs content

        Quoted ->
            el [] (text <| "\"" ++ content ++ "\"")


renderWithDictionary : RenderArgs -> String -> List Element -> E.Element msg
renderWithDictionary renderArgs name body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name body

        Nothing ->
            E.paragraph [ spacing 8 ]
                [ el [ Font.color (rgb255 200 0 0), Font.bold ] (text name)
                , el [] (text " ")
                , paragraph [] (renderList renderArgs body)
                ]



-- TEXT STYLE


italic : FRender msg
italic renderArgs _ body =
    paragraph [ Font.italic ] (renderList renderArgs body)


bold : FRender msg
bold renderArgs _ body =
    paragraph [ Font.bold ] (renderList renderArgs body)


strike : FRender msg
strike renderArgs _ body =
    paragraph [ Font.strike ] (renderList renderArgs body)


underline : FRender msg
underline renderArgs _ body =
    paragraph [ Font.underline ] (renderList renderArgs body)


hide : FRender msg
hide renderArgs _ body =
    E.none


highlight : FRender msg
highlight renderArgs _ body =
    paragraph [ Background.color yellowColor, E.paddingXY 4 2 ] (renderList renderArgs body)


bluelight : FRender msg
bluelight renderArgs _ body =
    paragraph [ Background.color lightBlueColor, E.paddingXY 4 2 ] (renderList renderArgs body)


redlight : FRender msg
redlight renderArgs _ body =
    paragraph [ Background.color lightRedColor, E.paddingXY 4 2 ] (renderList renderArgs body)


red : FRender msg
red renderArgs _ body =
    paragraph [ Font.color redColor ] (renderList renderArgs body)


error : FRender msg
error renderArgs _ body =
    paragraph [] [ paragraph [ Font.color redColor, E.paddingXY 4 4, Background.color (rgb255 242 199 226) ] (renderList renderArgs body), el [ E.paddingXY 2 4 ] (text " ") ]


blue : FRender msg
blue renderArgs _ body =
    paragraph [ Font.color blueColor ] (renderList renderArgs body)


violet : FRender msg
violet renderArgs _ body =
    paragraph [ Font.color violetColor ] (renderList renderArgs body)


gray : FRender msg
gray renderArgs _ body =
    paragraph [ Font.color (rgb 0.55 0.55 0.55) ] (renderList renderArgs body)


quoted : FRender msg
quoted renderArgs _ body =
    paragraph [] (renderList renderArgs body)


indent : FRender msg
indent renderArgs _ body =
    column [ indentPadding ] (renderList renderArgs body)


indentPadding =
    E.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


code1 : RenderArgs -> String -> E.Element msg
code1 renderArgs content =
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        ]
        (text <| " " ++ content)


codeblock : FRender msg
codeblock renderArgs _ body =
    column
        [ Font.family [ Font.typeface "Inconsolata", Font.monospace ]
        , Font.color codeColor
        , htmlAttribute "white-space" "pre"
        , spacing 8
        , paddingEach { left = 18, right = 0, top = 0, bottom = 0 }
        ]
        (List.map text (String.lines (String.replace " " (String.fromChar '\u{00A0}') (AST.stringContent body))))


monospace : E.Attribute msg
monospace =
    Font.family [ Font.typeface "Source Code Pro", Font.monospace ]


htmlAttribute : String -> String -> E.Attribute msg
htmlAttribute key value =
    E.htmlAttribute (HA.attribute key value)


code : FRender msg
code renderArgs _ body =
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        ]
        (text <| " " ++ AST.stringContent body)


fontRGB : FRender msg



-- fontRGB renderArgs _ body =


fontRGB renderArgs _ body =
    let
        ( args, realBody ) =
            AST.argsAndBody 3 body
    in
    case convertRGB args of
        Nothing ->
            el [ Font.color redColor ] (text "Bad RGB args")

        Just { r, b, g } ->
            paragraph [ Font.color (E.rgb255 r g b), E.paddingXY 4 2 ] (List.map (render renderArgs) realBody)


link : FRender msg
link renderArgs name body =
    case List.map AST.getText body of
        label :: url :: rest ->
            E.newTabLink []
                { url = url
                , label = el [ Font.color linkColor, Font.italic ] (text <| label)
                }

        _ ->
            E.el [] (text "Invalid link")


padLeft : E.Element msg -> E.Element msg
padLeft element =
    E.paragraph [] [ text " ", element ]



--    E.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


heading1 : FRender msg
heading1 renderArgs name body =
    column [ Font.size (headerFontSize 1), headerPadding 1 ] (renderList renderArgs body)


heading2 : FRender msg
heading2 renderArgs name body =
    column [ Font.size (headerFontSize 2), headerPadding 2 ] (renderList renderArgs body)


heading3 : FRender msg
heading3 renderArgs name body =
    column [ Font.size (headerFontSize 3), headerPadding 3 ] (renderList renderArgs body)


heading4 : FRender msg
heading4 renderArgs name body =
    column [ Font.size (headerFontSize 4), headerPadding 4 ] (renderList renderArgs body)


getFactor level =
    min 1.8 (sqrt (sqrt (toFloat level)))


headerFontSize level =
    round (24 / getFactor level)


headerPadding level =
    E.paddingEach { top = round (12 / getFactor level), bottom = 0, left = 0, right = 0 }


item : FRender msg
item renderArgs name body =
    column [ paddingEach { left = 24, right = 0, top = 0, bottom = 0 } ] (renderList renderArgs body)


image : FRender msg
image renderArgs name body =
    let
        args =
            body |> List.map AST.getText |> List.reverse

        url =
            List.head args |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict (List.drop 1 args)

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    E.none

                Just c ->
                    E.row [ placement, E.width E.fill ] [ el [ E.width E.fill ] (text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            E.px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    E.centerX

                Just "left" ->
                    E.alignLeft

                Just "right" ->
                    E.alignRight

                Just "center" ->
                    E.centerX

                _ ->
                    E.centerX

        displayWidth =
            renderArgs.width
    in
    E.column [ spacing 8, E.width (E.px displayWidth), placement ]
        [ E.image [ E.width width, placement ]
            { src = url, description = description }
        , caption
        ]


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


renderMathDisplay2 : FRender msg
renderMathDisplay2 rendArgs name body =
    mathText rendArgs DisplayMathMode (AST.stringContent body)


math2 : FRender msg
math2 renderArgs name body =
    mathText renderArgs InlineMathMode (AST.stringContent body)


mathblock : FRender msg
mathblock rendArgs name body =
    mathText rendArgs DisplayMathMode (AST.stringContent body)


renderMath : FRender msg
renderMath renderArgs name body =
    mathText renderArgs InlineMathMode (AST.stringContent body)


mathText : RenderArgs -> DisplayMode -> String -> E.Element msg
mathText renderArgs displayMode content =
    Html.Keyed.node "span"
        [ HA.style "margin-left" "6px" ]
        [ ( String.fromInt renderArgs.generation, mathText_ displayMode renderArgs.selectedId content )
        ]
        |> E.html


mathText_ : DisplayMode -> String -> String -> Html msg
mathText_ displayMode selectedId content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True



-- HELPERS


convertRGB : List String -> Maybe { r : Int, g : Int, b : Int }
convertRGB data =
    case data of
        r :: g :: b :: [] ->
            Just
                { r = String.toInt r |> Maybe.withDefault 0
                , g = String.toInt g |> Maybe.withDefault 0
                , b = String.toInt b |> Maybe.withDefault 0
                }

        _ ->
            Nothing


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


getText : Element -> Maybe String
getText element =
    case element of
        Text content _ ->
            Just content

        _ ->
            Nothing



-- COLORS


linkColor =
    E.rgb 0 0 0.8


blackColor =
    E.rgb 0 0 0


medGray =
    E.rgb 0.4 0.4 0.4


redColor =
    E.rgb 0.7 0 0


lightRedColor =
    E.rgb 1.0 0.8 0.8


blueColor =
    E.rgb 0 0 0.8


lightBlueColor =
    E.rgb 0.8 0.8 1.0


darkBlueColor =
    E.rgb 0 0 0.6


yellowColor =
    E.rgb 1.0 1.0 0


violetColor =
    E.rgb 0.4 0 0.8


codeColor =
    -- E.rgb 0.2 0.5 1.0
    E.rgb 0.4 0 0.8
