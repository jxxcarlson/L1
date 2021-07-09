module Main exposing (..)

import Browser
import Data
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Keyed
import Parser.Document
import Parser.Driver
import Render.Elm


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


type alias Flags =
    { width : Int, height : Int }


initialText =
    Data.initialText


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
    Render.Elm.renderList renderArgs (Parser.Driver.parse k str)


renderDocument : Int -> String -> List (List (Element Msg))
renderDocument generation document =
    document
        |> Parser.Document.parse generation
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
                | input = str
                , renderedText = render model.count str
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
        [ row [ spacing 12 ] [ clearTextButton ]
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



-- RENDER STRING TO HTML MSG


initState k =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    }


initStateWithData k data =
    { generation = k
    , blockOffset = 0
    , selectedId = ""
    , width = 300
    , parserData = data
    }


paragraphFormat =
    { maximumWidth = 80, optimalWidth = 70, stringWidth = String.length }


paragraphFormat2 =
    { maximumWidth = 160, optimalWidth = 150, stringWidth = String.length }



--keyedNode : Int -> List Html.Parser.Node -> Element msg
--keyedNode k element =
--    Html.Parser.Util.toVirtualDom element
--        |> keyIt k
--        |> Html.Keyed.node "div" []
--        |> Element.html


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

        --, label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
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
        , label = el [ centerX, centerY, Font.size 14 ] (text "clear")
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
    ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
