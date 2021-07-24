module L1.Handle exposing (doublePipe, heading2, item, lineCommand, pipe)

import L1.AST as AST exposing (Element(..), Name(..))
import L1.Config as Config exposing (Configuration, EType(..), Expectation)
import L1.MetaData as MetaData exposing (MetaData)
import L1.Parser as Parser
import L1.Stack as Stack exposing (StackItem)
import L1.TextCursor exposing (TextCursor)
import Library.Console as Console
import List.Extra


heading2 tc =
    let
        parsed_ =
            case Parser.parseHeading tc.generation tc.source of
                Ok goodstuff ->
                    goodstuff

                Err _ ->
                    errorElement tc.source
    in
    { tc | complete = parsed_ :: tc.complete }


errorElement message =
    EList
        [ Element (Name "redlight") (Text "Error " MetaData.dummy) MetaData.dummy
        , Element (Name "bluelight") (Text message MetaData.dummy) MetaData.dummy
        ]
        MetaData.dummy


item tc =
    let
        parsed_ =
            case Parser.parseItem tc.generation tc.source of
                Ok goodstuff ->
                    goodstuff

                Err _ ->
                    errorElement tc.source
    in
    { tc | complete = parsed_ :: tc.complete }


pipe tc =
    let
        source_ =
            "[" ++ String.trimLeft (String.dropLeft 1 tc.source) ++ "]"
    in
    { tc | complete = Parser.parse tc.generation source_ :: tc.complete }


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
                    Element (Name (String.trimLeft (String.dropLeft 2 name))) (Text body MetaData.dummy) MetaData.dummy
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
                List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]

            else
                List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]


lineCommand tc parsed_ =
    case List.Extra.uncons (List.reverse parsed_) of
        Nothing ->
            List.reverse tc.complete ++ [ Text "empty" MetaData.dummy ]

        Just ( first, [] ) ->
            case String.words (AST.getText first) of
                [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name "empty") (EList [] MetaData.dummy) MetaData.dummy ]

                name :: [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) (EList [] MetaData.dummy) MetaData.dummy ]

                name :: rest ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) (Text (String.join " " rest) MetaData.dummy) MetaData.dummy ]

        Just ( first, rest ) ->
            List.reverse tc.complete ++ [ Element (AST.Name (String.trim (AST.getText first))) (EList rest MetaData.dummy) MetaData.dummy ]
