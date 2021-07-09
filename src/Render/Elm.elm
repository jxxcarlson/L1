module Render.Elm exposing (convertString, render, renderList)

import Dict exposing (Dict)
import Element as E exposing (column, el, fill, paragraph, px, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Parser.AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Driver
import Parser.Error exposing (Context(..), Problem(..))
import Parser.MetaData as MetaData
import Utility


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

        --, ( "red", red )
        --, ( "blue", blue )
        --, ( "violet", violet )
        --, ( "medgray", medgray )
        --, ( "code", renderCode )
        --, ( "c", renderCode )
        , ( "math", renderMath )
        , ( "m", renderMath )
        , ( "mathblock", renderMathDisplay )
        , ( "mb", renderMathDisplay )
        , ( "link", link )
        , ( "image", image )
        ]


renderList : RenderArgs -> List Element -> List (E.Element msg)
renderList renderArgs list =
    List.map (render renderArgs) list


render : RenderArgs -> Element -> E.Element msg
render renderArgs element =
    case element of
        Raw str _ ->
            E.el [] (text str)

        Element (Name name) _ body _ ->
            renderWithDictionary renderArgs name [] body

        Element Undefined _ body _ ->
            E.el [] (text <| "Undefined element")

        EList elements _ ->
            E.paragraph [] (List.map (render renderArgs) elements)

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


fontRGB : FRender msg
fontRGB renderArgs _ _ body =
    let
        args2_ =
            args2 body

        args1_ =
            convertString (getText body |> Maybe.withDefault "none")

        _ =
            Debug.log "(args1, args2)" ( args1_, args2_ )
    in
    case ( args1_, args2_ ) of
        ( Nothing, Nothing ) ->
            el [ Font.color redColor ] (text "Error: bad or too few arguments to fontRGB")

        ( Just args, Nothing ) ->
            fontRGB_ renderArgs args

        ( Nothing, Just args ) ->
            fontRGB_ renderArgs args

        -- el [ Font.color redColor ] (text "Error: I can't explain this one.")
        ( Just _, Just _ ) ->
            el [ Font.color redColor ] (text "Error: I can't explain this one.")


fontRGB_ renderArgs args =
    let
        _ =
            Debug.log "fontRGB_ args" args
    in
    paragraph [ Font.color (E.rgb255 args.r args.g args.b), E.paddingXY 4 2 ] (List.map (render renderArgs) (Debug.log "REST!!" args.rest))


link : FRender msg
link renderArgs name args body =
    let
        bodyText =
            getText body |> Maybe.withDefault "missing url"

        ( label, url ) =
            case String.words bodyText of
                label_ :: url_ :: rest ->
                    ( label_, url_ )

                url_ :: [] ->
                    ( url_, url_ )

                [] ->
                    ( "no label", "https://nowhere.com" )
    in
    E.newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (text label)
        }


image : FRender msg
image renderArgs name _ body =
    let
        args_ =
            getText body |> Maybe.withDefault "" |> String.words

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


renderMathDisplay : FRender msg
renderMathDisplay rendArgs name args body =
    case getText body of
        Just content ->
            mathText rendArgs DisplayMathMode content

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


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
        []
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


args2 : Element -> Maybe { r : Int, g : Int, b : Int, rest : List Element }
args2 body =
    let
        _ =
            body |> Parser.AST.simplify |> Debug.log "ARGS2, BODY"
    in
    case body of
        EList list _ ->
            case list of
                (Raw r_ _) :: (Raw g_ _) :: (Raw b_ _) :: rest_ ->
                    Just { r = toInt r_, g = toInt g_, b = toInt b_, rest = rest_ }

                ((Raw str _) as raw) :: ((Element _ _ _ _) as elt) :: rest ->
                    let
                        aa =
                            convertString str

                        phrase =
                            String.words str |> List.drop 3 |> String.join " "
                    in
                    case aa of
                        Just a ->
                            Just
                                { r = a.r
                                , g = a.g
                                , b = a.b
                                , rest = Raw phrase MetaData.dummy :: elt :: rest

                                --, rest = elt :: rest
                                }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


convertString : String -> Maybe { r : Int, g : Int, b : Int, rest : List Element }
convertString str =
    case String.words str of
        r :: g :: b :: rest ->
            Just
                { r = String.toInt r |> Maybe.withDefault 0
                , g = String.toInt g |> Maybe.withDefault 0
                , b = String.toInt b |> Maybe.withDefault 0
                , rest = [ Raw (String.join " " rest) MetaData.dummy ]
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
        Raw s _ ->
            s

        _ ->
            ""


getText : Element -> Maybe String
getText element =
    case element of
        EList [ Raw content _ ] _ ->
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
