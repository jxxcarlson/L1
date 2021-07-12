module Parser.Config exposing
    ( Configuration
    , EType(..)
    , Expectation
    , MarkPosition(..)
    , configure
    , isBeginChar
    , isEndChar
    , lookup
    , name
    , notDelimiter
    )

import Dict exposing (Dict)
import List.Extra
import Maybe.Extra


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
    Dict Char Expectation


type alias ConfigurationDefinition =
    List Expectation


type alias Configuration =
    { beginSymbols : List String
    , endSymbols : List String
    , beginChars : List Char
    , interiorBeginChars : List Char
    , endChars : List Char
    , interiorEndChars : List Char
    , delimiters : List Char
    , interiorDelimiters : List Char
    , verbatimChars : List Char
    , expectationsDict : ExpectationsDict
    }


lookup : Configuration -> Char -> Maybe Expectation
lookup config c =
    Dict.get c config.expectationsDict


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
    in
    { beginSymbols = configDef |> List.map .beginSymbol
    , endSymbols = configDef |> List.map .endSymbol |> Maybe.Extra.values
    , beginChars = beginChars
    , interiorBeginChars = interiorBeginChars
    , endChars = endChars |> Maybe.Extra.values
    , interiorEndChars = interiorEndChars |> Maybe.Extra.values
    , delimiters = beginChars ++ (endChars |> Maybe.Extra.values) |> List.Extra.unique
    , interiorDelimiters = interiorBeginChars ++ (interiorEndChars |> Maybe.Extra.values) |> List.Extra.unique
    , verbatimChars = [] -- verbatimChars configDef
    , expectationsDict = Dict.empty
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
