module L1.Parser.Handle exposing (doublePipe, heading2, item, lineCommand, pipe)

import L1.Library.Console as Console
import L1.Library.Vector as Vector
import L1.Parser.AST as AST exposing (Element(..), Name(..))
import L1.Parser.Config as Config exposing (Configuration, EType(..), Expectation)
import L1.Parser.MetaData as MetaData exposing (MetaData)
import L1.Parser.Parser as Parser
import L1.Parser.Stack as Stack exposing (StackItem)
import L1.Parser.TextCursor as TextCursor exposing (TextCursor)
import List.Extra


heading2 : TextCursor -> TextCursor
heading2 tc =
    case Parser.parseHeading tc.generation tc.chunkLocation tc.previousScanPoint tc.source of
        Ok parsedHeading_ ->
            let
                headingLevel =
                    case AST.getName parsedHeading_ of
                        Nothing ->
                            0

                        Just name ->
                            case name of
                                "heading1" ->
                                    0

                                "heading2" ->
                                    1

                                "heading3" ->
                                    2

                                "heading4" ->
                                    3

                                _ ->
                                    4

                newAccumulator =
                    TextCursor.advanceHeadingRegister headingLevel tc.accumulator

                label =
                    newAccumulator.headingRegister |> Vector.toString

                parsedHeading =
                    AST.setLabel label parsedHeading_
            in
            { tc | complete = [ parsedHeading ] ++ tc.complete, accumulator = newAccumulator }

        Err _ ->
            { tc | complete = errorElement tc.source ++ tc.complete }


errorElement message =
    [ Element (Name "redlight") [ Text "Error " MetaData.dummy ] MetaData.dummy
    , Element (Name "bluelight") [ Text message MetaData.dummy ] MetaData.dummy
    ]


item tc =
    let
        parsed_ =
            case Parser.parseItem tc.generation tc.chunkLocation tc.previousScanPoint tc.source of
                Ok goodstuff ->
                    [ goodstuff ]

                Err _ ->
                    errorElement tc.source
    in
    { tc | complete = parsed_ ++ tc.complete }


pipe tc =
    let
        source_ =
            "[" ++ String.trimLeft (String.dropLeft 1 tc.source) ++ "]"
    in
    { tc | complete = Parser.parse tc.generation tc.chunkLocation tc.previousScanPoint source_ :: tc.complete }


doublePipe tc =
    let
        lines =
            String.lines tc.source
    in
    case List.head lines of
        Nothing ->
            { tc | complete = Text "Error, no content" MetaData.dummy :: tc.complete }

        Just name ->
            let
                body =
                    List.drop 1 lines |> String.join "\n"

                element =
                    Element (Name (String.trimLeft (String.dropLeft 2 name))) [ Text body MetaData.dummy ] MetaData.dummy
            in
            { tc | complete = element :: tc.complete }


headings : TextCursor -> StackItem -> List Element -> List Element
headings tc top_ parsed_ =
    case top_ of
        Stack.EndMark _ ->
            []

        Stack.TextItem _ ->
            []

        Stack.Expect top ->
            if List.member top.expect.beginSymbol [ "#", "##", "###" ] then
                List.reverse tc.complete ++ [ Element (AST.Name "heading") (List.reverse parsed_) MetaData.dummy ]

            else
                List.reverse tc.complete ++ [ Element (AST.Name "heading") (List.reverse parsed_) MetaData.dummy ]


lineCommand tc parsed_ =
    case List.Extra.uncons (List.reverse parsed_) of
        Nothing ->
            List.reverse tc.complete ++ [ Text "empty" MetaData.dummy ]

        Just ( first, [] ) ->
            case String.words (AST.getText first) of
                [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name "empty") [] MetaData.dummy ]

                name :: [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) [] MetaData.dummy ]

                name :: rest ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) [ Text (String.join " " rest) MetaData.dummy ] MetaData.dummy ]

        Just ( first, rest ) ->
            List.reverse tc.complete ++ [ Element (AST.Name (String.trim (AST.getText first))) rest MetaData.dummy ]
