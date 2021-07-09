module Parser.Config exposing
    ( Configuration
    , EType(..)
    , Expectation
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
    { begin : Char, end : Char, etype : EType, isVerbatim : Bool }


type EType
    = ElementType
    | CodeType
    | InlineMathType


name : EType -> String
name etype =
    case etype of
        ElementType ->
            "element"

        CodeType ->
            "code"

        InlineMathType ->
            "math2"


type alias ExpectationsDict =
    Dict Char Expectation


type alias ConfigurationDefinition =
    List Expectation


type alias Configuration =
    { beginChars : List Char
    , endChars : List Char
    , delimiters : List Char
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
            List.map .end configDef
    in
    { beginChars = beginChars
    , endChars = endChars
    , delimiters = beginChars ++ endChars |> List.Extra.unique
    , verbatimChars = List.filter (\def -> def.isVerbatim) configDef |> List.map .begin |> List.Extra.unique
    , expectationsDict = Dict.fromList (List.map (\e -> ( e.begin, e )) configDef)
    }


notDelimiter : Configuration -> Char -> Bool
notDelimiter config c =
    not (List.member c config.delimiters)


isBeginChar : Configuration -> Char -> Bool
isBeginChar config c =
    List.member c config.beginChars


isEndChar : Configuration -> Char -> Bool
isEndChar config c =
    List.member c config.endChars
