module L1.Parser.Loop exposing (nextCursor, parseLoop)

import L1.Library.Console as Console
import L1.Library.ParserTools as ParserTools exposing (StringData)
import L1.Library.Utility
import L1.Parser.AST as AST exposing (Element(..), Name(..))
import L1.Parser.Config as Config exposing (Configuration, EType(..))
import L1.Parser.Configuration as Configuration
import L1.Parser.Error exposing (Context, Problem)
import L1.Parser.Handle as Handle
import L1.Parser.Loc as Loc
import L1.Parser.Print
import L1.Parser.ShiftReduce exposing (Operation(..), ReduceOperation(..), ShiftOperation(..), operation)
import L1.Parser.Stack as Stack exposing (StackItem(..))
import L1.Parser.TextCursor as TextCursor exposing (ScannerType(..), TextCursor)
import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser a =
    Parser.Parser Context Problem a


{-| parseLoop scans the source text from right to left, update the TextCursor
on each pass. See module Parser.TextCursor for definitions. The TextCursor
is initialized with source text. When parseLoop concludes, it also carries
the AST of the processed source.
-}
parseLoop : (Int -> Loc.ChunkLocation -> Int -> String -> Element) -> Int -> Loc.ChunkLocation -> String -> TextCursor
parseLoop parser generation chunkLocation str =
    let
        result =
            ParserTools.loop (TextCursor.init generation chunkLocation str) (nextCursor parser)
                |> (\tc_ -> { tc_ | complete = List.reverse tc_.complete })

        --_ =
        --    Debug.log (L1.Parser.Print.print result) "-"
        --_ =
        --    Debug.log "(P, C)" ( cursor.previousScanPoint, cursor.scanPoint )
    in
    result


{-| nextCursor operates by advancing from one syntactic mark to another, e.g.,
'[' or ']' in the case of language L1. On each move it updates the cursor
with one of four TextCursor functions: `add`, `push`, `pop`, 'push'.

The scanPoint field of the text cursor points the character in source
field that is currently being scanned. As a convenience, cursor.remaining
holds the source text from the scanPoint onwards.

The scanPoint must be incremented by at least one unit on each pass so
that parseLoop is guaranteed to terminate. The program terminates
when the scanPoint comes to the end of the source.

-}
nextCursor : (Int -> Loc.ChunkLocation -> Int -> String -> Element) -> TextCursor -> ParserTools.Step TextCursor TextCursor
nextCursor parser cursor =
    let
        -- _ =
        -- Debug.log (L1.Parser.Print.print cursor) "-"
        _ =
            L1.Library.Utility.debug "STACK" cursor.stack
    in
    case operation cursor of
        Shift op ->
            shift op cursor

        Reduce op ->
            reduce op parser cursor


shift : ShiftOperation -> TextCursor -> ParserTools.Step TextCursor TextCursor
shift op cursor =
    case op of
        PushText str ->
            ParserTools.Loop
                { cursor
                    | count = cursor.count + 1
                    , stack = Stack.TextItem { content = str.content, position = { start = 0, end = String.length str.content } } :: cursor.stack
                    , previousScanPoint = cursor.scanPoint
                    , scanPoint = cursor.scanPoint + String.length str.content
                    , parsed = []
                    , message = "PUSH(t)"
                }

        PushSymbol data ->
            push cursor data


reduce : ReduceOperation -> (Int -> Loc.ChunkLocation -> Int -> String -> Element) -> TextCursor -> ParserTools.Step TextCursor TextCursor
reduce op parser cursor =
    case op of
        End ->
            ParserTools.Done { cursor | complete = cursor.parsed ++ cursor.complete, message = "COMMIT 1" }

        Commit ->
            ParserTools.Loop (TextCursor.commit parser { cursor | message = "COMMIT 2", count = cursor.count + 1 })

        HandleError ->
            ParserTools.Loop (TextCursor.commit parser { cursor | message = "COMMIT 3", count = cursor.count + 1 })

        Add strData ->
            ParserTools.Loop
                { cursor
                    | count = cursor.count + 1
                    , previousScanPoint = cursor.scanPoint
                    , scanPoint = cursor.scanPoint + String.length strData.content
                    , complete = parser cursor.generation cursor.chunkLocation cursor.previousScanPoint strData.content :: cursor.parsed ++ cursor.complete
                    , parsed = []
                    , message = "ADD" -- main
                }

        Pop prefix ->
            pop parser prefix cursor

        ShortCircuit str ->
            shortcircuit str cursor


pop parser prefix cursor =
    ParserTools.Loop <| TextCursor.pop parser prefix { cursor | message = "POP", scannerType = NormalScan }


push : TextCursor -> { prefix : String, isMatch : Bool } -> ParserTools.Step TextCursor TextCursor
push cursor ({ prefix, isMatch } as prefixData) =
    case Config.lookup Configuration.configuration prefix of
        Nothing ->
            ParserTools.Loop <|
                TextCursor.push prefixData
                    (TextCursor.EndMark_ prefix)
                    { cursor | message = "PUSH(e)" }

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
                TextCursor.push prefixData (TextCursor.Expect_ expectation) { cursor | message = "PUSH(s)", scannerType = scannerType }


shortcircuit : String -> TextCursor -> ParserTools.Step TextCursor TextCursor
shortcircuit prefix cursor =
    if List.member prefix [ "#", "##", "###", "####" ] then
        ParserTools.Done <| Handle.heading2 { cursor | message = "SHORT(h)" }

    else if prefix == ":" then
        ParserTools.Done <| Handle.item { cursor | message = "SHORT(:)" }

    else if prefix == "|" then
        ParserTools.Done <| Handle.pipe { cursor | message = "SHORT(|)" }

    else if prefix == "||" then
        ParserTools.Done <| Handle.doublePipe { cursor | message = "SHORT(||)" }

    else
        ParserTools.Done { cursor | message = "SHORT(?)" }


error cursor =
    ParserTools.Done { cursor | message = "ERROR" }
