module Render.Elm exposing (convertRGB, render, renderList)

import Dict exposing (Dict)
import Element as E exposing (column, el, fill, paddingEach, paragraph, px, rgb, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Parser.AST as AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Error exposing (Context(..), Problem(..))
import Parser.MetaData as MetaData
import Parser.Utility
import Utility.Utility as Utility


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    }


type alias FRender msg =
    RenderArgs -> String -> List String -> Element -> E.Element msg


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
        , ( "highlightRGB", highlight )
        , ( "fontRGB", fontRGB )
        , ( "red", red )
        , ( "blue", blue )
        , ( "violet", violet )
        , ( "gray", gray )
        , ( "code", code )
        , ( "quoted", quoted )
        , ( "math2", math2 )
        , ( "m", renderMath )
        , ( "mathblock", mathblock )
        , ( "mb", mathblock )
        , ( "link", link )
        , ( "image", image )
        , ( "heading", heading )
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
            renderWithDictionary renderArgs name [] body

        Element Undefined body _ ->
            E.el [] (text <| "Undefined element")

        EList elements _ ->
            E.paragraph [] (List.map (AST.map (\s -> " " ++ s) >> render renderArgs) elements)

        Problem _ str ->
            el [] (text <| "PROBLEM: " ++ str)

        StackError _ _ message errorText ->
            paragraph [] [ el [ Background.color (rgb255 255 255 0) ] (text errorText), el [ Font.bold, Font.color (rgb255 0 0 200) ] (text <| " " ++ message) ]

        Empty ->
            el [] (text <| "EMPTY")


renderWithDictionary renderArgs name args body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name args body

        Nothing ->
            E.paragraph [ spacing 8 ]
                [ el [ Font.color (rgb255 200 0 0), Font.bold ] (text name)
                , el [] (text " ")
                , render renderArgs body
                ]



-- TEXT STYLE


italic : FRender msg
italic renderArgs _ _ body =
    el [ Font.italic ] (render renderArgs body)


bold : FRender msg
bold renderArgs _ _ body =
    el [ Font.bold ] (render renderArgs body)


strike : FRender msg
strike renderArgs _ _ body =
    el [ Font.strike ] (render renderArgs body)


underline : FRender msg
underline renderArgs _ _ body =
    el [ Font.underline ] (render renderArgs body)


hide : FRender msg
hide renderArgs _ _ body =
    E.none


highlight : FRender msg
highlight renderArgs _ _ body =
    el [ Background.color yellowColor, E.paddingXY 4 2 ] (render renderArgs body)


red : FRender msg
red renderArgs _ _ body =
    el [ Font.color redColor ] (render renderArgs body)


blue : FRender msg
blue renderArgs _ _ body =
    el [ Font.color blueColor ] (render renderArgs body)


violet : FRender msg
violet renderArgs _ _ body =
    el [ Font.color violetColor ] (render renderArgs body)


gray : FRender msg
gray renderArgs _ _ body =
    el [ Font.color (rgb 0.55 0.55 0.55) ] (render renderArgs body)


quoted : FRender msg
quoted renderArgs _ _ body =
    render renderArgs body


code : FRender msg
code renderArgs _ _ body =
    el
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        ]
        (text <| " " ++ AST.getText body)


fontRGB : FRender msg
fontRGB renderArgs _ _ body =
    let
        colorArgs =
            List.take 3 (AST.toStringList body)

        textArgs =
            List.drop 3 (AST.toList body) |> List.map (AST.map (\x -> " " ++ x))
    in
    case convertRGB colorArgs of
        Nothing ->
            el [ Font.color redColor ] (text "Bad RGB args")

        Just { r, b, g } ->
            paragraph [ Font.color (E.rgb255 r g b), E.paddingXY 4 2 ] (List.map (render renderArgs) textArgs)


link : FRender msg
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
    E.newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (text <| Utility.unquote label)
        }
        |> padLeft


padLeft : E.Element msg -> E.Element msg
padLeft element =
    E.paragraph [] [ text " ", element ]



--    E.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


heading : FRender msg
heading renderArgs name args body =
    el [ Font.size 24 ] (render renderArgs body)


item : FRender msg
item renderArgs name args body =
    el [ paddingEach { left = 24, right = 0, top = 0, bottom = 0 } ] (render renderArgs body)


image : FRender msg
image renderArgs name _ body =
    let
        args_ =
            getText2 body |> String.words

        args =
            List.take (List.length args_ - 1) args_

        url =
            List.head (List.reverse args_) |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict args

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
renderMathDisplay2 rendArgs name args body =
    mathText rendArgs DisplayMathMode (getText2 body)


math2 : FRender msg
math2 renderArgs name args body =
    mathText renderArgs InlineMathMode (getText2 body)


mathblock : FRender msg
mathblock rendArgs name args body =
    mathText rendArgs DisplayMathMode (Debug.log "CONTENT" (AST.stringContent body))


renderMath : FRender msg
renderMath renderArgs name args body =
    case getText body of
        Just content ->
            mathText renderArgs InlineMathMode content

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


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
    e |> Debug.log "E" |> getText2 |> Debug.log "GE" |> toInt


getText2 : Element -> String
getText2 element =
    case element of
        Text s _ ->
            s

        EList list _ ->
            List.map getText2 list |> String.join " "

        _ ->
            ""


getText : Element -> Maybe String
getText element =
    case element of
        EList [ Text content _ ] _ ->
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


blueColor =
    E.rgb 0 0 0.8


darkBlueColor =
    E.rgb 0 0 0.6


yellowColor =
    E.rgb 1.0 1.0 0


violetColor =
    E.rgb 0.4 0 0.8


codeColor =
    -- E.rgb 0.2 0.5 1.0
    E.rgb 0.4 0 0.8
