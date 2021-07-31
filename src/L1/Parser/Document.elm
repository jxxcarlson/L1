module L1.Parser.Document exposing (parse, parseWithTOC)

import L1.Parser.AST as AST exposing (Element)
import L1.Parser.Chunk
import L1.Parser.Parser


type alias Document =
    String


d1 =
    """
AAA
BBB

CCC
DDD


FFF
GGG
"""


parseWithTOC : Int -> Document -> List (List Element)
parseWithTOC generation doc =
    let
        ast =
            parse generation doc

        title =
            List.take 1 ast

        toc =
            AST.makeTOC ast
    in
    List.take 3 ast ++ [ toc ] :: List.drop 3 ast


parse : Int -> Document -> List (List Element)
parse generation doc =
    let
        p : String -> List Element
        p =
            L1.Parser.Chunk.parse (L1.Parser.Parser.parse generation) generation
    in
    doc
        |> split generation
        |> List.map .content
        -- ^^^ Temporary
        |> List.map p


split : Int -> Document -> List { generation : Int, index : Int, firstLine : Int, content : String }
split generation doc =
    doc
        |> String.lines
        |> groupLines
        |> List.filter (\group -> group.content /= [])
        |> List.indexedMap (\index group -> { generation = generation, index = index, firstLine = group.lineNumber, content = String.join "\n" group.content })


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
