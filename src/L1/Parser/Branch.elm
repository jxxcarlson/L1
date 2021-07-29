module L1.Parser.Branch exposing (Operation(..), ReduceOperation(..), ShiftOperation(..), operation)

import L1.Library.Console as Console
import L1.Library.ParserTools as ParserTools exposing (StringData)
import L1.Library.Utility as Utility exposing (debug)
import L1.Parser.Config as Config exposing (Configuration)
import L1.Parser.Configuration as Configuration
import L1.Parser.Stack as Stack
import L1.Parser.TextCursor as TextCursor exposing (TextCursor)



--_ =
--    Debug.log (L1.Parser.Print.print cursor) ""


type Operation
    = Reduce ReduceOperation
    | Shift ShiftOperation


type ReduceOperation
    = End
    | Commit
    | HandleError
    | Add StringData
    | Pop String
    | ShortCircuit String


type ShiftOperation
    = PushText StringData
    | PushSymbol { prefix : String, isMatch : Bool }


operation : TextCursor -> Operation
operation cursor =
    if cursor.scanPoint >= cursor.sourceLength then
        if cursor.stack == [] then
            Reduce End

        else
            Reduce HandleError

    else
        let
            ( textToProcess, maybeFirstChar ) =
                getScanPointData cursor
        in
        case maybeFirstChar of
            Nothing ->
                Reduce End

            Just firstChar ->
                branch Configuration.configuration cursor firstChar (getPrefix firstChar textToProcess)


getPrefix : Char -> String -> String
getPrefix firstChar textToProcess =
    ParserTools.prefixWith firstChar textToProcess |> .content


getScanPointData : { a | scanPoint : Int, source : String } -> ( String, Maybe Char )
getScanPointData cursor =
    let
        textToProcess =
            String.dropLeft cursor.scanPoint cursor.source
    in
    ( textToProcess, textToProcess |> String.uncons |> Maybe.map Tuple.first )


branch : Config.Configuration -> TextCursor -> Char -> String -> Operation
branch configuration_ cursor firstChar prefix_ =
    let
        { value, prefix, isMatch } =
            canPush configuration_ cursor prefix_
    in
    if List.member prefix [ "|", "||", ":", "#", "##", "###", "####", "```" ] then
        Reduce (ShortCircuit prefix)

    else if
        Stack.isReducible cursor.stack
            && Maybe.map (Stack.beginSymbol >> Config.isVerbatimSymbol) (List.head cursor.stack)
            == Just True
    then
        Reduce (Pop prefix_)

    else if Config.notDelimiter Configuration.configuration Config.AllDelimiters firstChar then
        let
            chompedText =
                TextCursor.advance cursor (String.dropLeft cursor.scanPoint cursor.source)
        in
        if cursor.stack == [] then
            Reduce (Add chompedText)

        else
            Shift (PushText chompedText)

    else if value then
        Shift (PushSymbol { prefix = prefix, isMatch = isMatch })

    else if canPop configuration_ cursor prefix_ then
        Reduce (Pop prefix_)

    else
        Reduce Commit


{-| The parser has paused at character c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Configuration -> TextCursor -> String -> Bool
canPop configuration_ tc prefix =
    Stack.isReducibleWith prefix tc.stack


canPopPrecondition : Configuration -> TextCursor -> String -> Bool
canPopPrecondition configuration_ tc prefix =
    let
        isEndSymbol =
            Config.isEndSymbol configuration_ tc.scanPoint prefix
    in
    if isEndSymbol then
        True

    else if String.length prefix > 1 then
        canPopPrecondition configuration_ tc (String.dropLeft 1 prefix)

    else
        False


canPush : Configuration -> TextCursor -> String -> { value : Bool, prefix : String, isMatch : Bool }
canPush configuration_ tc prefix =
    if Config.isVerbatimSymbol prefix && prefixMatchesTopOfStack prefix tc.stack then
        -- If the symbol is a verbatim symbol following one that is
        -- on top of the stack the return True
        { value = True, prefix = prefix, isMatch = True }

    else
        -- Otherwise, the symbol is non-verbatim.  Check to see if it can be pushed
        canPushNonVerbatim configuration_ tc prefix


prefixMatchesTopOfStack prefix stack =
    Just prefix == (List.head stack |> Maybe.map Stack.beginSymbol)


canPushNonVerbatim : Configuration -> TextCursor -> String -> { value : Bool, prefix : String, isMatch : Bool }
canPushNonVerbatim configuration_ tc prefix =
    if prefix == "" then
        -- No prefix: terminate
        { value = False, prefix = "", isMatch = False }

    else if
        -- the prefix is a begin symbol OR it is an end symbol
        -- and the stack is not reducible with this prefix
        Config.isBeginSymbol configuration_ tc.scanPoint prefix
            || (Config.isEndSymbol configuration_ tc.scanPoint prefix
                    && Stack.isNotReducibleWith prefix tc.stack
                    && tc.stack
                    /= []
               )
    then
        { value = True, prefix = prefix, isMatch = False }

    else
        -- Try a substring.  A prefix might be "|" or "||", for example,
        -- So we try "||" and if that fails, we try "|"
        -- Since we are truncating the prefix, we need a termination condition;
        -- Hence 'if prefix == "' clause above.
        canPush configuration_ tc (String.dropLeft 1 prefix)
