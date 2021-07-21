module Parser.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), add, advance, advanceNormal, commit, pop, push
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Library.Console as Console
import Library.ParserTools as ParserTools
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced
import Parser.Config as Config exposing (Configuration, EType(..), Expectation)
import Parser.Configuration as Configuration
import Parser.MetaData as MetaData exposing (MetaData)
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
    let
        delimiterTypes =
            if List.member (String.slice 0 1 str) [ "|", ":" ] then
                Config.AllDelimiters

            else
                Config.InteriorDelimiters
    in
    case Parser.Advanced.run (ParserTools.text (Config.notDelimiter Configuration.configuration delimiterTypes) (Config.notDelimiter Configuration.configuration delimiterTypes)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


{-| Advance, but according to different criteria, because the
scanner type has been set to 'VerbatimScan c'
-}
advanceVerbatim : Char -> String -> ParserTools.StringData
advanceVerbatim verbatimChar str =
    let
        predicate =
            \c -> c /= verbatimChar

        -- && c /= ']'
    in
    case Parser.Advanced.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


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
push : { prefix : String, isMatch : Bool } -> ProtoStackItem -> TextCursor -> TextCursor
push ({ prefix, isMatch } as prefixData) proto tc =
    let
        newText : ParserTools.StringData
        newText =
            if isMatch then
                -- TODO: think about scanPoint
                { start = 0, finish = 0, content = "" }

            else
                advance tc (String.dropLeft (tc.scanPoint + String.length prefix) tc.source)

        newContent =
            newText.content

        scanPointIncrement =
            String.length prefix + newText.finish - newText.start

        newStack =
            case proto of
                Expect_ expectation ->
                    Stack.Expect
                        { expect = expectation
                        , content = newContent
                        , count = tc.count
                        , scanPoint = tc.scanPoint + scanPointIncrement
                        , position = { start = tc.scanPoint, end = tc.scanPoint + scanPointIncrement }
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
        , stack = newStack
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
    commit_ parse cursor |> (\tc -> { tc | complete = List.reverse tc.complete })


commit_ : (String -> Element) -> TextCursor -> TextCursor
commit_ parse tc =
    case tc.stack of
        [] ->
            finishUp tc

        top :: restOfStack ->
            if Stack.isReducible tc.stack then
                finishUpWithReducibleStack parse tc

            else
                resolveError parse tc top restOfStack


finishUp : TextCursor -> TextCursor
finishUp tc =
    let
        parsed =
            if tc.text == "" then
                tc.parsed

            else
                AST.Text tc.text MetaData.dummy :: tc.parsed

        complete =
            parsed ++ tc.complete
    in
    { tc | parsed = [], complete = complete }


finishUpWithReducibleStack parse tc =
    let
        stackData =
            tc.stack |> List.reverse |> List.map Stack.show |> String.join ""
    in
    { tc | complete = parse stackData :: tc.complete }



-- LANGUAGE HANDLERS


handleTheRest0 : (String -> Element) -> TextCursor -> StackItem -> List StackItem -> Element -> TextCursor
handleTheRest0 parse tc top restOfStack newParsed =
    let
        complete_ =
            case Stack.endSymbol top of
                Nothing ->
                    let
                        parsed_ =
                            newParsed :: tc.parsed
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


resolveError : (String -> Element) -> TextCursor -> StackItem -> List StackItem -> TextCursor
resolveError parse tc top restOfStack =
    case Stack.endSymbol top of
        Nothing ->
            let
                _ =
                    Debug.log (Console.magenta "RESOLVE ERROR") 1

                newParsed =
                    parse (Stack.showStack tc.stack)
            in
            commit parse
                { tc
                    | count = 1 + tc.count
                    , text = ""
                    , stack = restOfStack
                    , parsed = []
                    , complete = List.reverse (newParsed :: tc.parsed)
                }

        Just _ ->
            let
                _ =
                    Debug.log (Console.magenta "RESOLVE ERROR") 2

                _ =
                    Debug.log (Console.bgBlue "ERROR, stackTop") top

                errorPosition =
                    Stack.startPosition top

                errorContent =
                    Stack.content top |> Debug.log (Console.magenta "errorContent")

                goodText =
                    String.dropLeft (errorPosition + 1) tc.source |> Debug.log (Console.magenta "goodText")

                errorElement =
                    Element (Name "error") (Text (" unmatched " ++ Stack.beginSymbol top) MetaData.dummy) MetaData.dummy

                -- revisedTop =
                _ =
                    Debug.log (Console.bgBlue "ERROR POSITION (301)") (" Unmatched " ++ Stack.beginSymbol top ++ " at position " ++ String.fromInt (Stack.startPosition top))
            in
            { tc
                | count = 1 + tc.count
                , text = ""
                , stack = []
                , parsed = []
                , scanPoint = errorPosition + 1
                , complete = List.reverse (errorElement :: tc.parsed)
            }


handleError : TextCursor -> StackItem -> List Element
handleError tc top =
    let
        _ =
            Debug.log (Console.bgBlue "ERROR, stackTop") (List.head tc.stack)

        errorPosition =
            Stack.startPosition top

        _ =
            Debug.log (Console.bgBlue "ERROR POSITION (331)") (" Unmatched " ++ Stack.beginSymbol top ++ " at position " ++ String.fromInt (Stack.startPosition top))
    in
    List.reverse tc.complete
        ++ [ Element (Name "error") (Text (" unmatched " ++ Stack.beginSymbol top ++ " at position " ++ String.fromInt errorPosition) MetaData.dummy) MetaData.dummy
           , Text (Stack.beginSymbol top) MetaData.dummy
           ]
        ++ List.reverse tc.parsed
        ++ [ Text tc.text MetaData.dummy ]
