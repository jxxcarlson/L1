module Parser.Loop exposing (Packet, advance, parseLoop)

import Parser.AST as AST exposing (Element, simplify)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Config
import Parser.Error exposing (Context, Problem)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Tool


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : String -> a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor -> TextCursor)
    }


expectations =
    [ { begin = '[', end = ']' } ]


defaultExpectation =
    { begin = '@', end = '@' }


configuration =
    Parser.Config.configure expectations


{-| parseLoop scans the source text from right to left, update the TextCursor
on each pass. See module Parser.TextCursor for definitions. The TextCursor
is initialized with source text. When parseLoop concludes, it also carries
the AST of the processed source.
-}
parseLoop : Packet Element -> Int -> String -> TextCursor
parseLoop packet generation str =
    Parser.Tool.loop (TextCursor.init generation str) (nextCursor packet)
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
nextCursor : Packet Element -> TextCursor -> Parser.Tool.Step TextCursor TextCursor
nextCursor packet tc =
    if tc.offset >= tc.length then
        Parser.Tool.Done tc

    else
        let
            remaining =
                -- offset has been updated, so remaining should be also
                String.dropLeft tc.offset tc.remainingSource

            chompedText =
                -- get some more text
                -- this means text from one mark to the next
                advance remaining

            n =
                chompedText.finish - chompedText.start

            firstChar =
                String.uncons remaining |> Maybe.map Tuple.first
        in
        if n > 0 then
            -- the chompedText is non-void; add it it to the cursor
            Parser.Tool.Loop <| TextCursor.add chompedText.content tc

        else
            -- We are at a mark, and so must decide whether to push, pop, or call it quits.
            case firstChar of
                Nothing ->
                    Parser.Tool.Done tc

                Just c ->
                    if Parser.Config.isBeginChar configuration c then
                        Parser.Tool.Loop <| TextCursor.push packet.parser { begin = '[', end = ']' } tc

                    else if Parser.Config.isEndChar configuration c then
                        Parser.Tool.Loop <| TextCursor.pop packet.parser tc

                    else
                        Parser.Tool.Done tc


notDelimiter : Char -> Bool
notDelimiter c =
    not (List.member c [ '[', ']' ])


advance : String -> Parser.Tool.StringData
advance str =
    case Parser.run (Parser.Tool.text (\c -> notDelimiter c) (\c -> notDelimiter c)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }
