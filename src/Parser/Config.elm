module Parser.Config exposing
    ( Configuration
    , EType(..)
    , Expectation
    , MarkPosition(..)
    , configure
    , isBeginChar
    , isBeginSymbol
    , isEndChar
    , isEndSymbol
    , lookup
    , name
    , notDelimiter
    )

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Utility.Utility


type alias Expectation =
    { beginSymbol : String
    , endSymbol : Maybe String
    , etype : EType
    , isVerbatim : Bool
    , markPosition : MarkPosition
    }


type EType
    = ElementType
    | CodeType
    | InlineMathType
    | QuotedType


type MarkPosition
    = AtBeginning
    | Anywhere


name : EType -> String
name etype =
    case etype of
        ElementType ->
            "element"

        CodeType ->
            "code"

        InlineMathType ->
            "math2"

        QuotedType ->
            "quoted"


type alias ExpectationsDict =
    Dict String Expectation


type alias ConfigurationDefinition =
    List Expectation


type alias Configuration =
    { beginSymbols : List String
    , endSymbols : List String
    , beginChars : List Char
    , interiorBeginChars : List Char
    , interiorBeginSymbols : List String
    , endChars : List Char
    , interiorEndChars : List Char
    , interiorEndSymbols : List String
    , delimiters : List Char
    , interiorDelimiters : List Char
    , verbatimChars : List Char
    , expectationsDict : ExpectationsDict
    }


lookup : Configuration -> String -> Maybe Expectation
lookup config prefix =
    Dict.get prefix config.expectationsDict


firstChar : String -> Maybe Char
firstChar str =
    case String.uncons str of
        Nothing ->
            Nothing

        Just ( c, _ ) ->
            Just c


configure : ConfigurationDefinition -> Configuration
configure configDef =
    let
        beginChars =
            List.map (.beginSymbol >> firstChar) configDef |> Maybe.Extra.values

        endChars =
            List.map (.endSymbol >> Maybe.map firstChar) configDef |> Maybe.Extra.values

        interiorBeginChars =
            List.map (.beginSymbol >> firstChar) (List.filter (\e -> e.markPosition == Anywhere) configDef)
                |> Maybe.Extra.values

        interiorEndChars =
            List.map (.endSymbol >> Maybe.map firstChar) (List.filter (\e -> e.markPosition == Anywhere) configDef)
                |> Maybe.Extra.values

        interiorBeginSymbols =
            List.map .beginSymbol (List.filter (\e -> e.markPosition == Anywhere) configDef)

        interiorEndSymbols =
            List.map .endSymbol (List.filter (\e -> e.markPosition == Anywhere) configDef)
                |> Maybe.Extra.values
    in
    { beginSymbols = configDef |> List.map .beginSymbol
    , endSymbols = configDef |> List.map .endSymbol |> Maybe.Extra.values
    , beginChars = beginChars
    , interiorBeginSymbols = interiorBeginSymbols
    , interiorBeginChars = interiorBeginChars
    , endChars = endChars |> Maybe.Extra.values
    , interiorEndChars = interiorEndChars |> Maybe.Extra.values
    , interiorEndSymbols = interiorEndSymbols
    , delimiters = beginChars ++ (endChars |> Maybe.Extra.values) |> List.Extra.unique
    , interiorDelimiters = interiorBeginChars ++ (interiorEndChars |> Maybe.Extra.values) |> List.Extra.unique
    , verbatimChars = [] -- verbatimChars configDef
    , expectationsDict = Dict.fromList (List.map (\e -> ( e.beginSymbol, e )) configDef)
    }



--verbatimChars : List Configuration -> List Configuration
--verbatimChars configDef =
--    configDef |> List.filter (\e -> e.isVerbatim)
--|> List.map firstChar


notDelimiter : Configuration -> Int -> Char -> Bool
notDelimiter config position c =
    if position == 0 then
        not (List.member c config.delimiters)

    else
        not (List.member c config.interiorDelimiters)


isBeginChar : Configuration -> Int -> Char -> Bool
isBeginChar config position c =
    if position == 0 then
        List.member c config.beginChars

    else
        List.member c config.interiorBeginChars


isEndChar : Configuration -> Int -> Char -> Bool
isEndChar config position c =
    if position == 0 then
        List.member c config.endChars

    else
        List.member c config.interiorEndChars


isBeginSymbol : Configuration -> Int -> String -> Bool
isBeginSymbol config position str =
    if position == 0 then
        List.member str config.beginSymbols

    else
        List.member str config.interiorBeginSymbols


isEndSymbol : Configuration -> Int -> String -> Bool
isEndSymbol config position str =
    if position == 0 then
        List.member str config.endSymbols

    else
        List.member str config.interiorEndSymbols
