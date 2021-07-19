module Parser.Loop exposing (Packet, nextCursor, parseLoop)

import Library.Console as Console
import Library.ParserTools as ParserTools
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Branch as Branch exposing (Operation(..), branch)
import Parser.Config as Config exposing (Configuration, EType(..))
import Parser.Configuration as Configuration
import Parser.Error exposing (Context, Problem)
import Parser.Print
import Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor)


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
parseLoop : (String -> Element) -> Int -> String -> TextCursor
parseLoop parser generation str =
    let
        result =
            ParserTools.loop (TextCursor.init generation str) (nextCursor parser)
                |> TextCursor.commit parser
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
nextCursor : (String -> Element) -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor parser cursor =
    if cursor.count > 15 then
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
                case branch Configuration.configuration cursor firstChar prefix of
                    ADD ->
                        add parser cursor chompedText

                    PUSH prefix_ ->
                        push cursor prefix_

                    POP ->
                        pop parser prefix cursor

                    COMMIT ->
                        done cursor "No alternative"


done cursor message =
    ParserTools.Done { cursor | message = message }


add parser cursor chompedText =
    ParserTools.Loop <| TextCursor.add parser chompedText.content { cursor | message = "ADD" }


pop parser prefix cursor =
    ParserTools.Loop <| TextCursor.pop parser prefix { cursor | message = "POP", scannerType = NormalScan }


push cursor prefix =
    case Config.lookup Configuration.configuration prefix of
        Nothing ->
            ParserTools.Loop <| TextCursor.push (Debug.log (Console.cyan "PUSHING MARK with prefix") prefix) (TextCursor.EndMark_ prefix) { cursor | message = "PUSH Endmark " ++ prefix }

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
            ParserTools.Loop <|
                TextCursor.push (Debug.log (Console.cyan "PUSHING with prefix") prefix) (TextCursor.Expect_ expectation) { cursor | message = "PUSH", scannerType = scannerType }


error cursor =
    ParserTools.Done { cursor | message = "Unexpected error: no corresponding expectation" }
