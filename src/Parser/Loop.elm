module Parser.Loop exposing (Packet, advance, parseLoop)

import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Config exposing (Configuration, EType(..))
import Parser.Configuration as Configuration
import Parser.Error exposing (Context, Problem)
import Parser.MetaData as MetaData
import Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor)
import Utility.ParserTools as ParserTools
import Utility.Utility as Utility


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

        _ =
            Debug.log (TextCursor.print result) "-"
    in
    result


{-| nextCursor operates by advancing from one syntactic mark to another, e.g.,
'[' or ']' in the case of language L1. On each move it updates the cursor
with one of four TextCursor functions: `add`, `push`, `pop`, 'push'.

The offset field of the text cursor points the character in source
field that is currently being scanned. As a convenience, tc.remaining
holds the source text from the offset onwards.

The offset must be incremented by at least one unit on each pass so
that parseLoop is guaranteed to terminate. The program terminates
when the offset comes to the end of the source.

-}
nextCursor : Packet Element -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor packet cursor =
    let
        _ =
            Debug.log (TextCursor.print cursor) ""
    in
    if cursor.offset >= cursor.length then
        ParserTools.Done cursor

    else
        let
            remaining =
                -- offset has been updated, so remaining should be also
                String.dropLeft cursor.offset cursor.source

            chompedText =
                -- get some more text
                -- this means text from one mark to the next
                case cursor.scannerType of
                    NormalScan ->
                        advance configuration cursor.offset remaining

                    VerbatimScan c ->
                        advanceVerbatim2 c remaining
        in
        if chompedText.finish - chompedText.start > 0 then
            -- the chompedText is non-void; add it to the cursor
            ParserTools.Loop <| TextCursor.add chompedText.content { cursor | message = "ADD" }

        else
            -- We are at a mark, and so must decide whether to push, pop, or call it quits
            -- Decide this on the basis of the character at the heading the remaining text
            case String.uncons remaining |> Maybe.map Tuple.first of
                Nothing ->
                    ParserTools.Done cursor

                Just c ->
                    handleCharacterAtCursor packet c cursor


handleCharacterAtCursor : Packet Element -> Char -> TextCursor -> ParserTools.Step TextCursor TextCursor
handleCharacterAtCursor packet c tc =
    if TextCursor.canPop tc c then
        ParserTools.Loop <| TextCursor.pop packet.parser { tc | message = "POP" }
        --else

    else if Parser.Config.isBeginChar configuration tc.offset c then
        case Parser.Config.lookup configuration c of
            Nothing ->
                ParserTools.Done tc

            Just expectation ->
                let
                    scannerType =
                        if List.member expectation.etype [ CodeType, InlineMathType, QuotedType ] then
                            VerbatimScan c

                        else
                            NormalScan
                in
                ParserTools.Loop <| TextCursor.push packet.parser expectation { tc | message = "PUSH", scannerType = scannerType }

    else if Just c == (List.head tc.stack |> Maybe.andThen (.expect >> .expectedEndChar)) then
        ParserTools.Loop <| TextCursor.pop packet.parser { tc | message = "POP", scannerType = NormalScan }

    else
        -- TODO: add error message for unexpected end char
        ParserTools.Done tc


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


advanceVerbatim2 : Char -> String -> ParserTools.StringData
advanceVerbatim2 verbatimChar str =
    let
        predicate =
            \c -> c /= verbatimChar
    in
    case Parser.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }
