module Parser.Loop exposing (Packet, nextCursor, parseLoop)

import Library.Console as Console
import Library.ParserTools as ParserTools
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Config as Config exposing (Configuration, EType(..))
import Parser.Configuration as Configuration
import Parser.Error exposing (Context, Problem)
import Parser.Print
import Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor, configuration)


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : String -> a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor -> TextCursor)
    }


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
            Debug.log (Parser.Print.print result) "-"
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
    if cursor.count > 5 then
        done cursor "count exceeded"

    else
        let
            _ =
                Debug.log (Parser.Print.print cursor) ""

            textToProcess =
                String.dropLeft cursor.scanPoint cursor.source

            chompedText =
                TextCursor.advance cursor textToProcess

            maybeFirstChar =
                String.uncons textToProcess |> Maybe.map Tuple.first

            -- |> Debug.log (Console.magenta "FIRST CH")
            maybePrefix =
                Maybe.map ((\c -> ParserTools.prefixWith c textToProcess) >> .content) maybeFirstChar
        in
        case ( maybeFirstChar, maybePrefix ) of
            ( Nothing, _ ) ->
                done cursor "No prefix"

            ( _, Nothing ) ->
                done cursor "No first character"

            ( Just firstChar, Just prefix ) ->
                if Config.notDelimiter configuration 0 firstChar then
                    add cursor chompedText

                else if TextCursor.canPush configuration cursor prefix then
                    let
                        _ =
                            Debug.log (Console.magenta "WILL PUSH, prefix") prefix
                    in
                    push cursor prefix

                else if TextCursor.canPop configuration cursor prefix then
                    pop packet cursor

                else
                    done cursor "No alternative"


done cursor message =
    ParserTools.Done { cursor | message = message }


add cursor chompedText =
    ParserTools.Loop <| TextCursor.add chompedText.content { cursor | message = "ADD" }


pop packet cursor =
    ParserTools.Loop <| TextCursor.pop packet.parser { cursor | message = "POP", scannerType = NormalScan }


push cursor prefix =
    case Config.lookup configuration prefix of
        Nothing ->
            ParserTools.Loop <| TextCursor.push (TextCursor.EndMark_ prefix) { cursor | message = "PUSH Endmark " ++ prefix }

        Just expectation ->
            let
                scannerType =
                    -- Set the scanner type
                    if List.member expectation.etype [ CodeType, InlineMathType, QuotedType ] then
                        VerbatimScan (expectation.beginSymbol |> Config.firstChar |> Maybe.withDefault '0')
                        -- TODO: fix this (prefix)

                    else
                        NormalScan
            in
            ParserTools.Loop <| TextCursor.push (TextCursor.Expect_ expectation) { cursor | message = "PUSH", scannerType = scannerType }


error cursor =
    ParserTools.Done { cursor | message = "Unexpected error: no corresponding expectation" }
