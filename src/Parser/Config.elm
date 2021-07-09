module Parser.Config exposing (Configuration, configure, isBeginChar, isEndChar, lookup)

import Dict exposing (Dict)
import Maybe.Extra


type alias Expectation =
    { begin : Char, end : Char }


type alias ExpectationsDict =
    Dict Char Expectation


type alias ConfigurationDefinition =
    List Expectation


type alias Configuration =
    { beginChars : List Char
    , endChars : List Char
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
    , expectationsDict = Dict.fromList (List.map (\e -> ( e.begin, e )) configDef)
    }


isBeginChar : Configuration -> Char -> Bool
isBeginChar config c =
    List.member c config.beginChars


isEndChar : Configuration -> Char -> Bool
isEndChar config c =
    List.member c config.endChars



--expectationsDict : ExpectationsDict
--expectationsDict =
--    Dict.fromList [ ( '[', { begin = '[', end = ']' } ) ]
