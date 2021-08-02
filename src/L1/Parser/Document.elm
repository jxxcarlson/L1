module L1.Parser.Document exposing (d1, d2, parse, parseWithTOC, split)

import L1.Library.ParserTools as ParserTools
import L1.Parser.AST as AST exposing (Element)
import L1.Parser.Chunk
import L1.Parser.Loc as Loc
import L1.Parser.Loop as Loop
import L1.Parser.Parser
import L1.Parser.TextCursor as TextCursor exposing (Accumulator, TextCursor, emptyAccumulator)


d1 =
    """
AA
BB

CC
DD


EE
FF



GG
HH
"""


d2 =
    """
foo [b bar]

[i x] [b y]
"""


type alias Document =
    String


parseWithTOC : Int -> Document -> List (List Element)
parseWithTOC generation doc =
    let
        ast =
            parse generation doc

        toc =
            AST.makeTOC ast
    in
    List.map (AST.replaceByName toc) ast


parse : Int -> Document -> List (List Element)
parse generation doc =
    parseAux generation doc |> .output |> List.reverse


parseAux : Int -> Document -> { output : List (List Element), accumulator : Accumulator }
parseAux generation doc =
    List.foldl (folder generation) { output = [], accumulator = emptyAccumulator } (split doc)


folder : Int -> { location : Loc.ChunkLocation, content : String } -> { output : List (List Element), accumulator : Accumulator } -> { output : List (List Element), accumulator : Accumulator }
folder generation input acc =
    let
        newCursor =
            Loop.parseLoop L1.Parser.Parser.parse acc.accumulator generation input.location input.content
    in
    { output = newCursor.complete :: acc.output, accumulator = newCursor.accumulator }


{-|

    > split d1
    [{ content = "AA\nBB", location = { firstLine = 1, index = 0 } },{ content = "CC\nDD", location = { firstLine = 4, index = 1 } },{ content = "EE\nFF", location = { firstLine = 8, index = 2 } },{ content = "GG\nHH", location = { firstLine = 13, index = 3 } }]

-}
split : Document -> List { location : Loc.ChunkLocation, content : String }
split doc =
    doc
        |> String.lines
        |> groupLines
        |> List.filter (\group -> group.content /= [])
        |> List.indexedMap (\index group -> { location = { chunkIndex = index, firstLine = group.lineNumber }, content = String.join "\n" group.content })


type Status
    = InParagraph
    | BlankLines Int


type alias State =
    { status : Status
    , buffer : List String
    , output : List { content : List String, lineNumber : Int }
    , input : List String
    , lineNumber : Int
    , firstLineOfChunk : Int
    }


groupLines : List String -> List { content : List String, lineNumber : Int }
groupLines lines =
    groupLinesAux { status = InParagraph, buffer = [], output = [], input = lines, lineNumber = 0, firstLineOfChunk = 0 } |> .output |> List.reverse


groupLinesAux : State -> State
groupLinesAux state =
    case List.head state.input of
        Nothing ->
            { state | output = { content = List.reverse state.buffer, lineNumber = state.lineNumber } :: state.output }

        Just line ->
            case ( line == "", state.status ) of
                ( True, BlankLines n ) ->
                    -- ADD the current blank lines to the ones already found
                    groupLinesAux
                        { state
                            | buffer = line :: state.buffer
                            , status = BlankLines (n + 1)
                            , input = List.drop 1 state.input
                            , lineNumber = state.lineNumber + 1
                        }

                ( True, InParagraph ) ->
                    -- A blank line has been found following a non-blank line; the paragraph has ended
                    groupLinesAux
                        { state
                            | output = { content = List.reverse state.buffer, lineNumber = state.firstLineOfChunk } :: state.output
                            , buffer = []
                            , status = BlankLines 1
                            , input = List.drop 1 state.input
                            , lineNumber = state.lineNumber + 1
                        }

                ( False, BlankLines _ ) ->
                    -- A non-blank line has been found following one ore more blank lines; start a paragraph
                    groupLinesAux
                        { state
                            | buffer = [ line ]
                            , status = InParagraph
                            , input = List.drop 1 state.input
                            , lineNumber = state.lineNumber + 1
                            , firstLineOfChunk = state.lineNumber
                        }

                ( False, InParagraph ) ->
                    -- A non-blank line has been found following one or more blank lines; add it to the buffer
                    groupLinesAux
                        { state
                            | buffer = line :: state.buffer
                            , input = List.drop 1 state.input
                            , lineNumber = state.lineNumber + 1
                        }
