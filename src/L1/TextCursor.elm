module L1.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), add, advance, advanceNormal, commit, pop, push
    )

{-| TextCursor is the data structure used by L1.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import L1.AST as AST exposing (Element(..), Name(..))
import L1.Config as Config exposing (Configuration, EType(..), Expectation)
import L1.Configuration as Configuration
import L1.MetaData as MetaData exposing (MetaData)
import L1.Stack as Stack exposing (StackItem)
import Library.Console as Console
import Library.ParserTools as ParserTools
import List.Extra
import Parser.Advanced


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
        _ =
            Debug.log (Console.yellow "HERE IS") "TC.push"

        newText : ParserTools.StringData
        newText =
            (if isMatch then
                -- TODO: think about scanPoint
                { start = 0, finish = 0, content = "" }

             else
                advance tc (String.dropLeft (tc.scanPoint + String.length prefix) tc.source)
            )
                |> Debug.log (Console.yellow "newText")

        newContent =
            newText.content

        scanPointIncrement =
            case proto of
                Expect_ _ ->
                    String.length prefix + newText.finish - newText.start

                EndMark_ _ ->
                    if newContent == "" then
                        String.length prefix

                    else
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
                        -- TODO: need real position
                        Debug.log (Console.yellow "PUSH END MARK (1)") <| Stack.EndMark { content = prefix_, position = { start = tc.scanPoint, end = tc.scanPoint + scanPointIncrement } } :: tc.stack

                    else
                        Debug.log (Console.yellow "PUSH END MARK (2)") <| Stack.TextItem { content = newContent, position = { start = tc.scanPoint + 1, end = tc.scanPoint + String.length newContent } } :: Stack.EndMark { content = prefix_, position = { start = -1, end = -1 } } :: tc.stack
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
                    let
                        _ =
                            Debug.log (Console.magenta "POP BR") 1
                    in
                    handlePop parse prefix stackTop_ cursor

                Stack.TextItem data ->
                    let
                        _ =
                            Debug.log (Console.magenta "POP BR") ( 2, data, String.slice data.position.start data.position.end cursor.source )
                    in
                    { cursor
                        | count = cursor.count + 1

                        -- , scanPoint = cursor.scanPoint + String.length data.content - 1
                    }

                Stack.EndMark _ ->
                    let
                        _ =
                            Debug.log (Console.magenta "POP BR") 3
                    in
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
    commit_ parse cursor



-- TODO: do we need this above ?? ^^ ---|> (\tc -> { tc | complete = List.reverse tc.complete })


commit_ : (String -> Element) -> TextCursor -> TextCursor
commit_ parse tc =
    case tc.stack of
        [] ->
            finishUp tc

        _ ->
            -- if Stack.isReducible tc.stack then
            if Stack.isStrictlyReducible tc.stack then
                finishUpWithReducibleStack parse tc

            else
                -- CAN NOW ASSUME THAT THE STACK IS NOT REDUCIBLE
                resolveError tc


finishUp : TextCursor -> TextCursor
finishUp tc =
    let
        _ =
            Debug.log (Console.yellow "finishUp") "!!"

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
        _ =
            Debug.log (Console.yellow "finishUpWithReducibleStack") "!!"

        stackData =
            tc.stack |> List.reverse |> List.map Stack.show |> String.join ""
    in
    { tc | complete = parse stackData :: tc.complete }



-- LANGUAGE HANDLERS


resolveError : TextCursor -> TextCursor
resolveError tc =
    let
        _ =
            Debug.log (Console.magenta "RESOLVE ERROR") 2

        _ =
            Debug.log (Console.magenta "STACK STATE") (Stack.showStack tc.stack)

        maybeBottomOfStack =
            List.Extra.unconsLast tc.stack
                |> Maybe.map Tuple.first
                |> Debug.log (Console.bgBlue "badStackItem")

        errorPosition : Int
        errorPosition =
            maybeBottomOfStack
                |> Maybe.map Stack.startPosition
                |> Maybe.withDefault -1
                |> Debug.log (Console.bgBlue "errorPosition")

        _ =
            List.map (\item -> Stack.startPosition item) tc.stack |> Debug.log (Console.bgBlue "start positions")

        badStackItemSymbol =
            maybeBottomOfStack
                |> Maybe.map Stack.beginSymbol
                |> Maybe.withDefault "??"
                -- TODO: the above is hacky and DANGEROUS
                |> Debug.log (Console.bgBlue "badStackItem, beginSymbol")

        --errorContent =
        --    Stack.content top |> Debug.log (Console.magenta "errorContent")
        goodText =
            String.dropLeft (errorPosition + 1) tc.source |> Debug.log (Console.magenta "goodText")

        errorElement =
            Element (Name "error") (Text (" unmatched " ++ badStackItemSymbol) MetaData.dummy) MetaData.dummy
    in
    { tc
        | count = 1 + tc.count
        , text = ""
        , stack = []
        , parsed = []
        , scanPoint = errorPosition + 1
        , complete = errorElement :: tc.complete
    }
