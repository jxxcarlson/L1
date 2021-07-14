module Parser.Loop exposing (Packet, advance, nextCursor, parseLoop)

import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Config exposing (Configuration, EType(..))
import Parser.Configuration as Configuration
import Parser.Error exposing (Context, Problem)
import Parser.Print
import Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor)
import Utility.ParserTools as ParserTools


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : String -> a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor -> TextCursor)
    }


configuration =
    Parser.Config.configure Configuration.expectations


{-| parseLoop scans the source text from right to left, update the TextCursor
on each pass. See module Parser.TextCursor for definitions. The TextCursor
is initialized with source text. When parseLoop concludes, it also carries
the AST of the processed source.
-}
parseLoop : Packet Element -> Int -> String -> TextCursor
parseLoop packet generation str =
    let
        result =
            ParserTools.loop (TextCursor.init generation str) (nextCursor packet)
                |> TextCursor.commit
                |> (\tc_ -> { tc_ | message = "COMM" })

        --_ =
        --    Debug.log (Parser.Print.print result) "-"
    in
    result


{-| nextCursor operates by advancing from one syntactic mark to another, e.g.,
'[' or ']' in the case of language L1. On each move it updates the cursor
with one of four TextCursor functions: `add`, `push`, `pop`, 'push'.

The scanPoint field of the text cursor points the character in source
field that is currently being scanned. As a convenience, tc.remaining
holds the source text from the scanPoint onwards.

The scanPoint must be incremented by at least one unit on each pass so
that parseLoop is guaranteed to terminate. The program terminates
when the scanPoint comes to the end of the source.

-}
nextCursor : Packet Element -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor packet cursor =
    --let
    --    _ =
    --        Debug.log (Parser.Print.print cursor) ""
    --in
    if cursor.scanPoint >= cursor.length then
        ParserTools.Done cursor

    else
        let
            textToProcess =
                String.dropLeft cursor.scanPoint cursor.source

            chompedText =
                -- get some more text
                -- this means text from one mark to the next, not inclusive
                case cursor.scannerType of
                    NormalScan ->
                        advance configuration cursor.scanPoint textToProcess

                    VerbatimScan c ->
                        advanceVerbatim c textToProcess
        in
        if chompedText.finish - chompedText.start > 0 then
            -- the chompedText is non-void; add it to the cursor
            ParserTools.Loop <| TextCursor.add chompedText.content { cursor | message = "ADD" }

        else
            -- We are at a mark, and so must decide whether to push, pop, or commit
            -- Decide this on the basis of the prefix at the head of the text to process
            -- The prefix is longest prefix of the text to process consisting
            -- entirely of the character at the scanpoint
            case String.uncons textToProcess |> Maybe.map Tuple.first of
                Nothing ->
                    ParserTools.Done { cursor | message = "Unexpected error" }

                Just c ->
                    -- Get the prefix at the scanPoint using ParserTools.prefixWith,
                    -- then call 'handleCursor' to push, pop, or commit
                    handleCursor packet (ParserTools.prefixWith c textToProcess).content cursor


handleCursor : Packet Element -> String -> TextCursor -> ParserTools.Step TextCursor TextCursor
handleCursor packet prefix tc =
    -- POP
    if TextCursor.canPop configuration tc prefix then
        ParserTools.Loop <| TextCursor.pop packet.parser { tc | message = "POP", scannerType = NormalScan }

    else if TextCursor.canPush configuration tc prefix then
        -- PUSH or quit with error
        case Parser.Config.lookup configuration prefix of
            Nothing ->
                ParserTools.Done { tc | message = "Unexpected error: no corresponding expectation" }

            Just expectation ->
                let
                    scannerType =
                        -- Set the scanner type
                        if List.member expectation.etype [ CodeType, InlineMathType, QuotedType ] then
                            VerbatimScan (expectation.beginSymbol |> Parser.Config.firstChar |> Maybe.withDefault '0')
                            -- TODO: fix this (prefix)

                        else
                            NormalScan
                in
                ParserTools.Loop <| TextCursor.push expectation { tc | message = "PUSH", scannerType = scannerType }

    else
        ParserTools.Done { tc | message = "Could neither push nor pop.  What the heck?" }


{-| Return the longest prefix of str that does not contain a delimiter.
The delimiter sets used depend upon position. One set for position = 0,
another for position /= 0.
-}
advance : Configuration -> Int -> String -> ParserTools.StringData
advance config position str =
    case Parser.run (ParserTools.text (Parser.Config.notDelimiter configuration position) (Parser.Config.notDelimiter configuration position)) str of
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
    in
    case Parser.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }
