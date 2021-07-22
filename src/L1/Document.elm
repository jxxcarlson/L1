module L1.Document exposing (groupLines, parse, split)

import L1.AST exposing (Element)
import L1.Chunk
import L1.Parser


type alias Document =
    String


parse : Int -> Document -> List (List Element)
parse generation doc =
    let
        p : String -> List Element
        p =
            L1.Chunk.parse (L1.Parser.parse generation) generation
    in
    doc
        |> split
        |> List.map p


split : Document -> List String
split doc =
    doc
        |> String.lines
        |> groupLines
        |> List.map (String.join "\n")


splitOLD : Document -> List String
splitOLD doc =
    String.split "\n\n" doc
        |> List.filter (\s -> s /= "")
        |> List.map String.trim



--
--split2 :Document -> List String
--split2 doc =
--    doc
--      |> String.lines
--      |> groupLines


type Status
    = InParagraph
    | BlankLines Int


type alias State =
    { status : Status, buffer : List String, output : List (List String), input : List String }


groupLines : List String -> List (List String)
groupLines lines =
    groupLinesAux { status = InParagraph, buffer = [], output = [], input = lines } |> .output |> List.reverse


groupLinesAux : State -> State
groupLinesAux state =
    case List.head state.input of
        Nothing ->
            { state | output = List.reverse state.buffer :: state.output }

        Just line ->
            case ( line == "", state.status ) of
                ( True, BlankLines n ) ->
                    -- ADD the current blank lines to the ones already found
                    groupLinesAux { state | buffer = line :: state.buffer, status = BlankLines (n + 1), input = List.drop 1 state.input }

                ( True, InParagraph ) ->
                    -- A blank line has been found following a non-blank line; the paragraph has ended
                    groupLinesAux { state | output = List.reverse state.buffer :: state.output, buffer = [], status = BlankLines 1, input = List.drop 1 state.input }

                ( False, BlankLines _ ) ->
                    -- A non-blank line has been found following one ore more blank linse; start a paragraph
                    groupLinesAux { state | buffer = [ line ], status = InParagraph, input = List.drop 1 state.input }

                ( False, InParagraph ) ->
                    -- A non-blank line has been found following one or more blank lines; add it to the buffer
                    groupLinesAux { state | buffer = line :: state.buffer, input = List.drop 1 state.input }
