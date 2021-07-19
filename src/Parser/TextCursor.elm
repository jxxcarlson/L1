module Parser.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), add, advance, advanceNormal, commit, handleHeadings2, handleItem, pop, push
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Library.Console as Console
import Library.ParserTools as ParserTools
import Library.Utility
import List.Extra
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced
import Parser.Config as Config exposing (Configuration, EType(..), Expectation)
import Parser.Configuration as Configuration
import Parser.MetaData as MetaData exposing (MetaData)
import Parser.Parser as Parser
import Parser.Stack as Stack exposing (StackItem)


{-| TODO: give an account of what these fields do
-}
type alias TextCursor =
    { count : Int
    , generation : Int
    , scanPoint : Int
    , length : Int

    ---
    , source : String
    , text : String
    , parsed : List Element -- might be incorporated later
    , complete : List Element -- no changes will be made
    , stack : List StackItem -- a stack of unclosed elements
    , scannerType : ScannerType

    ---
    , message : String
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


type ProtoStackItem
    = Expect_ Expectation
    | EndMark_ String


{-| initialize with source text
-}
init : Int -> String -> TextCursor
init generation source =
    { count = 0
    , generation = generation

    --
    , scanPoint = 0
    , length = String.length source
    , scannerType = NormalScan

    --
    , source = source
    , text = ""
    , parsed = []
    , complete = []
    , stack = []

    --
    , message = "STAR"
    }


advance : TextCursor -> String -> ParserTools.StringData
advance cursor textToProcess =
    case cursor.scannerType of
        NormalScan ->
            advanceNormal Configuration.configuration
                cursor.scanPoint
                textToProcess

        VerbatimScan c ->
            advanceVerbatim c textToProcess


{-| Return the longest prefix of str that does not contain a delimiter.
The delimiter sets used depend upon position. One set for position = 0,
another for position /= 0.
-}
advanceNormal : Configuration -> Int -> String -> ParserTools.StringData
advanceNormal config position str =
    (let
        _ =
            Debug.log (Console.cyan "advanceNormal, str") str

        delimiterTypes =
            if String.slice 0 1 str == ":" then
                Config.AllDelimiters

            else
                Config.InteriorDelimiters
     in
     case Parser.Advanced.run (ParserTools.text (Config.notDelimiter Configuration.configuration delimiterTypes) (Config.notDelimiter Configuration.configuration delimiterTypes)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }
    )
        |> Debug.log (Console.yellow "advanceNormal from " ++ String.fromInt position)


{-| Advance, but according to different criteria, because the
scanner type has been set to 'VerbatimScan c'
-}
advanceVerbatim : Char -> String -> ParserTools.StringData
advanceVerbatim verbatimChar str =
    (let
        predicate =
            \c -> c /= verbatimChar && c /= ']'
     in
     case Parser.Advanced.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }
    )
        |> Debug.log (Console.yellow "advanceVerbatim with char " ++ String.fromChar verbatimChar)


add : (String -> Element) -> String -> TextCursor -> TextCursor
add parse_ str tc =
    { tc
        | count = tc.count + 1

        -- , stack = TextItem { content = str } :: tc.stack
        , scanPoint = tc.scanPoint + String.length str
        , complete = parse_ str :: tc.parsed ++ tc.complete
        , parsed = []
    }


{-| A
-}
push : String -> ProtoStackItem -> TextCursor -> TextCursor
push prefix proto tc =
    let
        newText =
            -- TODO: think about scanPoint
            advance tc (String.dropLeft (tc.scanPoint + String.length prefix) tc.source)

        --|> Debug.log (Console.magenta "NEW TEXT")
        newContent =
            newText.content |> Debug.log (Console.yellow "newContent")

        scanPointIncrement =
            String.length prefix + newText.finish - newText.start |> Debug.log (Console.yellow "PUSH, scanpoint increment")

        --  |> Debug.log "scanPointIncrement"
        newStack =
            case proto of
                Expect_ expectation ->
                    Stack.Expect
                        { expect = expectation
                        , content = newContent
                        , count = tc.count
                        , scanPoint = tc.scanPoint + scanPointIncrement
                        }
                        :: tc.stack

                EndMark_ prefix_ ->
                    if newContent == "" then
                        Stack.EndMark prefix_ :: tc.stack

                    else
                        Stack.TextItem { content = newContent } :: Stack.EndMark prefix_ :: tc.stack
    in
    { tc
        | count = tc.count + 1
        , scanPoint = tc.scanPoint + scanPointIncrement
        , stack = newStack -- |> Debug.log (Console.magenta "PUSH, STACK")
    }


pop : (String -> Element) -> String -> TextCursor -> TextCursor
pop parse prefix cursor =
    -- The cursors' scanPoint is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    --let
    --    _ =
    --        Debug.log (Console.cyan "POP, prefix") prefix
    --in
    let
        _ =
            Debug.log (Console.cyan "POP, prefix") prefix
    in
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1, scannerType = NormalScan }

        Just stackTop_ ->
            case stackTop_ of
                Stack.Expect _ ->
                    handlePop parse prefix stackTop_ cursor

                Stack.TextItem _ ->
                    { cursor | count = cursor.count + 1 }

                Stack.EndMark _ ->
                    handlePop parse prefix stackTop_ cursor


handlePop : (String -> Element) -> String -> StackItem -> TextCursor -> TextCursor
handlePop parse prefix stackTop cursor =
    let
        data =
            Stack.showStack (List.reverse cursor.stack) ++ prefix
    in
    { cursor
        | stack = []
        , parsed = parse data :: cursor.parsed
        , count = cursor.count + 1
        , scanPoint = cursor.scanPoint + 1
    }


commit : (String -> Element) -> TextCursor -> TextCursor
commit parse cursor =
    cursor |> commit_ parse |> (\tc2 -> { tc2 | complete = List.reverse tc2.complete })


commit_ : (String -> Element) -> TextCursor -> TextCursor
commit_ parse tc =
    let
        parsed =
            if tc.text == "" then
                tc.parsed

            else
                AST.Text tc.text MetaData.dummy :: tc.parsed

        newParsed =
            parse (Stack.showStack tc.stack)

        complete =
            parsed ++ tc.complete
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                _ =
                    Debug.log (Console.yellow "TOP") <| Stack.beginSymbol top
            in
            if Stack.isReducible tc.stack then
                handledUnfinished parse tc

            else
                handleTheRest parse tc top restOfStack newParsed



-- LANGUAGE HANDLERS


handleTheRest parse tc top restOfStack newParsed =
    let
        complete_ =
            case Stack.endSymbol top of
                Nothing ->
                    let
                        parsed_ =
                            newParsed :: tc.parsed

                        --|> Debug.log (Console.magenta "parsed_")
                    in
                    List.reverse parsed_

                Just _ ->
                    handleError tc top
    in
    commit parse
        { tc
            | count = 1 + tc.count
            , text = ""
            , stack = restOfStack
            , parsed = []
            , complete = complete_
        }


handledUnfinished parse tc =
    let
        stackData =
            Debug.log (Console.bgBlue "COMM, stack") (tc.stack |> List.reverse |> List.map Stack.show |> String.join "")
    in
    { tc | complete = parse stackData :: tc.complete }


handleHeadings2 tc =
    let
        parsed_ =
            case Parser.parseHeading tc.generation tc.source of
                Ok goodstuff ->
                    goodstuff

                Err _ ->
                    Text ("Error on '" ++ tc.source ++ "'") MetaData.dummy
    in
    { tc | complete = parsed_ :: tc.complete }


handleItem tc =
    let
        parsed_ =
            case Parser.parseItem tc.generation tc.source of
                Ok goodstuff ->
                    goodstuff

                Err _ ->
                    Text ("Error on '" ++ tc.source ++ "'") MetaData.dummy
    in
    { tc | complete = parsed_ :: tc.complete }


handleHeadings : TextCursor -> StackItem -> List Element -> List Element
handleHeadings tc top_ parsed_ =
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


handleLineCommand tc parsed_ =
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


handleError : TextCursor -> StackItem -> List Element
handleError tc top =
    List.reverse tc.complete
        ++ [ Element (Name "error") (Text (" unmatched " ++ Stack.beginSymbol top ++ " ") MetaData.dummy) MetaData.dummy
           , Text (Stack.beginSymbol top) MetaData.dummy
           ]
        ++ List.reverse tc.parsed
        ++ [ Text tc.text MetaData.dummy ]
