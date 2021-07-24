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
import L1.Stack as Stack exposing (StackItem(..))
import Library.Console as Console
import Library.ParserTools as ParserTools
import Library.Utility exposing (debug)
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
    , verbatimPrefix : Maybe String
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
    , verbatimPrefix = Nothing
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
    let
        _ =
            debug "add, tc.scanpoint" tc.scanPoint

        _ =
            debug "add, String.length str" ( str, String.length str )
    in
    if tc.stack == [] then
        { tc
            | count = tc.count + 1
            , scanPoint = tc.scanPoint + String.length str
            , complete = parse_ str :: tc.parsed ++ tc.complete
            , parsed = []
        }

    else
        { tc
            | count = tc.count + 1
            , stack = TextItem { content = str, position = { start = 0, end = String.length str } } :: tc.stack
            , scanPoint = tc.scanPoint + String.length str
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
            case proto of
                Expect_ _ ->
                    String.length prefix + newText.finish - newText.start

                EndMark_ _ ->
                    if newContent == "" then
                        String.length prefix

                    else
                        String.length prefix + newText.finish - newText.start

        ( verbatimPrefix, scannerType ) =
            case ( Config.isVerbatimSymbol prefix, Just prefix == tc.verbatimPrefix ) of
                -- TURN ON VERBATIM SCANNING: Fresh verbatim prefix
                ( True, False ) ->
                    case String.uncons prefix of
                        Nothing ->
                            ( Nothing, NormalScan )

                        Just ( ch, _ ) ->
                            ( Just prefix, VerbatimScan ch )

                -- TURN OFF VERBATIM SCANNING: second occurrence of a verbatim prefix
                ( True, True ) ->
                    ( Nothing, NormalScan )

                _ ->
                    ( tc.verbatimPrefix, tc.scannerType )

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
                        Stack.EndMark { content = prefix_, position = { start = tc.scanPoint, end = tc.scanPoint + scanPointIncrement } } :: tc.stack

                    else
                        Stack.TextItem { content = newContent, position = { start = tc.scanPoint + 1, end = tc.scanPoint + String.length newContent } } :: Stack.EndMark { content = prefix_, position = { start = -1, end = -1 } } :: tc.stack
    in
    { tc
        | count = tc.count + 1
        , verbatimPrefix = verbatimPrefix
        , scannerType = scannerType
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
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1, scannerType = NormalScan }

        Just stackTop_ ->
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


resolveError : TextCursor -> TextCursor
resolveError tc =
    let
        maybeBottomOfStack =
            List.Extra.unconsLast tc.stack
                |> Maybe.map Tuple.first

        errorPosition : Int
        errorPosition =
            maybeBottomOfStack
                |> Maybe.map Stack.startPosition
                |> Maybe.withDefault -1

        badStackItemSymbol =
            maybeBottomOfStack
                |> Maybe.map Stack.beginSymbol
                |> Maybe.withDefault "??"

        -- TODO: the above is hacky and DANGEROUS
        errorElement =
            Element (Name "error") (Text (" unmatched '" ++ badStackItemSymbol ++ "'") MetaData.dummy) MetaData.dummy
    in
    { tc
        | count = 1 + tc.count
        , text = ""
        , stack = []
        , parsed = []
        , scanPoint = errorPosition + 1
        , complete = errorElement :: tc.complete
    }
