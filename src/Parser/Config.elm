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


type alias Expectation =
    { begin : Char, end : Maybe Char, etype : EType, isVerbatim : Bool, markPosition : MarkPosition }


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
    { beginChars : List Char
    , endChars : List Char
    , delimiters : List Char
    , interiorDelimiters : List Char
    , verbatimChars : List Char
    , expectationsDict : ExpectationsDict
    }


lookup : Configuration -> Char -> Maybe Expectation
lookup config c =
    Dict.get c config.expectationsDict


configure : ConfigurationDefinition -> Configuration
configure configDef =
    let
        beginChars =
            List.map .begin configDef

        endChars =
            List.map .end configDef |> List.map (\e -> Maybe.withDefault '0' e) |> List.filter (\c -> c /= '0')

        interiorBeginChars =
            List.map .begin (List.filter (\e -> e.markPosition == Anywhere) configDef)

        interiorEndChars =
            List.map .end (List.filter (\e -> e.markPosition == Anywhere) configDef)
                |> List.map (\e -> Maybe.withDefault '0' e)
                |> List.filter (\c -> c /= '0')
    in
    { beginChars = beginChars
    , endChars = endChars
    , delimiters = beginChars |> List.Extra.unique
    , interiorDelimiters = interiorBeginChars ++ interiorEndChars |> List.Extra.unique
    , verbatimChars = List.filter (\def -> def.isVerbatim) configDef |> List.map .begin |> List.Extra.unique
    , expectationsDict = Dict.fromList (List.map (\e -> ( e.begin, e )) configDef)
    }


notDelimiter : Configuration -> Int -> Char -> Bool
notDelimiter config position c =
    if position == 0 then
        not (List.member c config.delimiters)

    else
        not (List.member c config.interiorDelimiters)


isBeginChar : Configuration -> Char -> Bool
isBeginChar config c =
    List.member c config.beginChars


isEndChar : Configuration -> Char -> Bool
isEndChar config c =
    List.member c config.endChars
