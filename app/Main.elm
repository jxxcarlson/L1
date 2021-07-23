module Main exposing (..)

import Browser
import Data.Article
import Data.Example
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File.Download as Download
import Html exposing (Html)
import L1.Chunk
import L1.Document
import L1.Parser
import Render.Elm
import Render.Markdown


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , renderedText : List (Element Msg)
    , count : Int
    , windowHeight : Int
    , windowWidth : Int
    }


type Msg
    = NoOp
    | InputText String
    | ClearText
    | LoadDocumentText String
    | ExportMarkdown


type alias Flags =
    { width : Int, height : Int }


initialText =
    Data.Example.text


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = initialText
      , renderedText = render 0 initialText
      , count = 0
      , windowHeight = flags.height
      , windowWidth = flags.width
      }
    , Cmd.none
    )


render : Int -> String -> List (Element Msg)
render k str =
    Render.Elm.renderList renderArgs (L1.Chunk.parse (L1.Parser.parse k) k str)


renderDocument : Int -> String -> List (List (Element Msg))
renderDocument generation document =
    document
        |> L1.Document.parse generation
        |> List.map (\para -> Render.Elm.renderList { renderArgs | generation = generation } para)


renderArgs =
    { width = 450
    , selectedId = "foobar"
    , generation = 0
    }


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model
                | input = String.trim str
                , renderedText = render model.count (String.trim str)
                , count = model.count + 1
              }
            , Cmd.none
            )

        ClearText ->
            ( { model
                | input = ""
                , renderedText = render model.count ""
                , count = model.count + 1
              }
            , Cmd.none
            )

        LoadDocumentText text ->
            ( { model | input = text }, Cmd.none )

        ExportMarkdown ->
            ( model, exportMarkdown model "article" )


exportMarkdown : Model -> String -> Cmd msg
exportMarkdown model fileName =
    let
        exportData =
            toMarkdown model.input
    in
    download (fileName ++ ".md") "text/markdown" exportData


toMarkdown : String -> String
toMarkdown content =
    Render.Markdown.transformDocument content


download : String -> String -> String -> Cmd msg
download filename mimetype content =
    Download.string filename mimetype content



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column (mainColumnStyle model)
        [ column [ spacing 48, width (px appWidth_), height (px (appHeight_ model)) ]
            [ title "D"
            , column [ spacing 12 ]
                [ row [ spacing 12 ] [ inputElement model, outputDisplay model ]
                ]
            , row [ Font.size 14, Font.color whiteColor ] []
            ]
        ]


inputElement model =
    column [ spacing 8, moveUp 9 ]
        [ row [ spacing 12 ] [ exampleDocButton, articleButton, exportToMarkdownButton ]
        , inputText model
        ]


title : String -> Element msg
title str =
    row [ centerX, fontGray 0.9 ] [ text str ]


outputDisplay : Model -> Element Msg
outputDisplay model =
    column [ spacing 8 ]
        [ row
            [ fontGray 0.9
            , spacing 12
            , moveUp 9
            , Font.size 14
            ]
            [ dummyButton, text ("generation: " ++ String.fromInt model.count), wordCountElement model.input ]
        , outputDisplay_ model
        ]


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


wordCountElement : String -> Element Msg
wordCountElement str =
    row [ spacing 8 ] [ el [] (text <| "words:"), el [] (text <| String.fromInt <| wordCount <| str) ]


outputDisplay_ : Model -> Element Msg
outputDisplay_ model =
    column
        [ spacing 18
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , paddingXY 24 36
        , width (px panelWidth_)
        , height (px (panelHeight_ model))
        , scrollbarY
        , moveUp 9
        , Font.size 12
        ]
        (List.map (\para -> paragraph [] para) (renderDocument model.count model.input))


keyIt : Int -> List b -> List ( String, b )
keyIt k list =
    List.indexedMap (\i e -> ( String.fromInt (i + k), e )) list



-- INPUT


inputText : Model -> Element Msg
inputText model =
    Input.multiline [ height (px (panelHeight_ model)), width (px panelWidth_), Font.size 14 ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelHidden "Enter source text here"
        , spellcheck = False
        }



-- BUTTONS


defaultButtonColor =
    Element.rgb255 60 60 60


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


clearTextButton : Element Msg
clearTextButton =
    Input.button buttonStyle2
        { onPress = Just ClearText
        , label = el [ centerX, centerY, Font.size 14 ] (text "Clear")
        }


exportToMarkdownButton : Element Msg
exportToMarkdownButton =
    Input.button buttonStyle2
        { onPress = Just ExportMarkdown
        , label = el [ centerX, centerY, Font.size 14 ] (text "Export: Markdown")
        }


exampleDocButton : Element Msg
exampleDocButton =
    Input.button buttonStyle2
        { onPress = Just (LoadDocumentText Data.Example.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "Examples")
        }


articleButton : Element Msg
articleButton =
    Input.button buttonStyle2
        { onPress = Just (LoadDocumentText Data.Article.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "Article")
        }


dummyButton : Element Msg
dummyButton =
    row [ Background.color defaultButtonColor ]
        [ Input.button buttonStyle
            { onPress = Nothing
            , label = el [ centerX, centerY, Font.size 14 ] (text "Rendered text")
            }
        ]



-- PARAMETERS


widePanelWidth_ =
    2 * panelWidth_


panelWidth_ =
    520


appHeight_ model =
    model.windowHeight - 300


panelHeight_ model =
    appHeight_ model - parserDisplayPanelHeight_ - 100


parserDisplayPanelHeight_ =
    0


appWidth_ =
    2 * panelWidth_ + 15



--
-- STYLE
--


mainColumnStyle model =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    , width (px (appWidth_ + 40))
    , height (px (appHeight_ model + 40))
    ]


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


buttonStyle2 =
    [ Font.color (rgb255 255 255 255)
    , Background.color (rgb255 0 0 180)
    , paddingXY 15 8
    , mouseDown [ Background.color (rgb255 180 180 255) ]
    ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
