module Parser.Loop exposing (Packet, advance, advanceVerbatim, parseLoop)

import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Config exposing (Configuration, EType(..))
import Parser.Error exposing (Context, Problem)
import Parser.MetaData as MetaData
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Utility.ParserTools as ParserTools


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : String -> a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor -> TextCursor)
    }


expectations =
    [ { begin = '[', end = ']', etype = ElementType, isVerbatim = False }
    , { begin = '`', end = '`', etype = CodeType, isVerbatim = True }
    , { begin = '$', end = '$', etype = InlineMathType, isVerbatim = True }
    ]


configuration =
    Parser.Config.configure expectations


{-| parseLoop scans the source text from right to left, update the TextCursor
on each pass. See module Parser.TextCursor for definitions. The TextCursor
is initialized with source text. When parseLoop concludes, it also carries
the AST of the processed source.
-}
parseLoop : Packet Element -> Int -> String -> TextCursor
parseLoop packet generation str =
    ParserTools.loop (TextCursor.init generation str) (nextCursor packet)
        |> TextCursor.commit


{-| nextCursor operates by advancing from one syntactic mark to another, e.g.,
'[' or ']' in the case of language L1. On each move it updates the cursor
with one of four TextCursor functions: `add`, `push`, `pop`, 'commit'.

The offset field of the text cursor points the character in source
field that is currently being scanned. As a convenience, tc.remaining
holds the source text from the offset onwards.

The offset must be incremented by at least one unit on each pass so
that parseLoop is guaranteed to terminate. The program terminates
when the offset comes to the end of the source.

-}
nextCursor : Packet Element -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor packet cursor =
    if cursor.offset >= cursor.length then
        ParserTools.Done cursor

    else
        let
            remaining =
                -- offset has been updated, so remaining should be also
                String.dropLeft cursor.offset cursor.remainingSource
                    |> Debug.log "nextCursor, remaining"

            chompedText =
                -- get some more text
                -- this means text from one mark to the next
                advance configuration remaining |> Debug.log "CHOMPED TEXT"
        in
        if chompedText.finish - chompedText.start > 0 then
            -- the chompedText is non-void; add it it to the cursor
            ParserTools.Loop <| TextCursor.add chompedText.content cursor

        else
            -- We are at a mark, and so must decide whether to push, pop, or call it quits
            -- Decide this on the basis of the character at the heading the remaining text
            case String.uncons remaining |> Maybe.map Tuple.first of
                Nothing ->
                    ParserTools.Done cursor

                Just c ->
                    handleCharacterAtCursor packet c cursor


handleCharacterAtCursor packet c tc =
    if Parser.Config.isBeginChar configuration c then
        case Parser.Config.lookup configuration c of
            Nothing ->
                ParserTools.Done tc

            Just expectation ->
                case expectation.etype of
                    CodeType ->
                        handleVerbatim CodeType tc

                    InlineMathType ->
                        handleVerbatim InlineMathType tc

                    _ ->
                        ParserTools.Loop <| TextCursor.push packet.parser expectation tc

    else if Parser.Config.isEndChar configuration c then
        ParserTools.Loop <| TextCursor.pop packet.parser tc

    else
        ParserTools.Done tc


handleVerbatim : EType -> TextCursor -> ParserTools.Step TextCursor TextCursor
handleVerbatim etype tc =
    let
        name =
            Parser.Config.name etype

        remaining_ =
            String.dropLeft (tc.offset + 1) tc.remainingSource

        verbatimText =
            advanceVerbatim configuration remaining_ |> Debug.log "VERBATIM TEXT"

        verbatimTextLength =
            verbatimText.finish - verbatimText.start |> Debug.log "VERBATIM TEXT LENGTH"

        preceding =
            Raw tc.text MetaData.dummy

        newElement =
            Element (Name name) (Raw verbatimText.content MetaData.dummy) MetaData.dummy

        newTC =
            { tc
                | offset = tc.offset + verbatimTextLength + 2
                , text = ""
                , parsed = newElement :: preceding :: tc.parsed
            }
    in
    ParserTools.Loop <| newTC


advance : Configuration -> String -> ParserTools.StringData
advance config str =
    case Parser.run (ParserTools.text (Parser.Config.notDelimiter configuration) (Parser.Config.notDelimiter configuration)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


advanceVerbatim : Configuration -> String -> ParserTools.StringData
advanceVerbatim config str =
    let
        verbatimChars =
            config.verbatimChars

        predicate =
            \c -> not (List.member c verbatimChars)
    in
    (case Parser.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData |> Debug.log "!!! ADVANCE VERBATIM"

        Err _ ->
            { content = "", finish = 0, start = 0 }
    )
        |> Debug.log "ADVANCE VERBATIM"
