module L1.Parser.Document exposing (d1, split)

-- import L1.Parser.AST as AST exposing (Element)
-- import L1.Parser.Chunk

import L1.Parser.Loc as Loc



--  import L1.Parser.Parser


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


type alias Document =
    String



--
--parseWithTOC : Int -> Int -> Document -> List (List Element)
--parseWithTOC generation count doc =
--    let
--        ast =
--            parse generation count doc
--
--        title =
--            List.take 1 ast
--
--        toc =
--            AST.makeTOC ast
--    in
--    List.take 3 ast ++ [ toc ] :: List.drop 3 ast
--
--
--parse : Int -> Int -> Document -> List (List Element)
--parse generation count doc =
--    let
--        p : String -> List Element
--        p =
--            L1.Parser.Chunk.parse (L1.Parser.Parser.parse generation count) generation
--    in
--    doc
--        |> split generation
--        |> List.map p


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
        |> List.indexedMap (\index group -> { location = { index = index, firstLine = group.lineNumber }, content = String.join "\n" group.content })


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
