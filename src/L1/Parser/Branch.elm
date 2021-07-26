module L1.Parser.Branch exposing (Operation(..), branch)

import L1.Library.Console as Console
import L1.Library.Utility exposing (debug)
import L1.Parser.Config as Config exposing (Configuration)
import L1.Parser.Configuration as Configuration
import L1.Parser.Stack as Stack
import L1.Parser.TextCursor exposing (TextCursor)


type Operation
    = ADD
    | PUSH { prefix : String, isMatch : Bool }
    | POP
    | SHORTCIRCUIT
    | COMMIT


branch : Config.Configuration -> TextCursor -> Char -> String -> Operation
branch configuration_ tc firstChar prefix_ =
    let
        { value, prefix, isMatch } =
            canPush configuration_ tc prefix_
    in
    if List.member prefix [ "|", "||", ":", "#", "##", "###", "####", "```" ] then
        SHORTCIRCUIT

    else if
        Stack.isReducible tc.stack
            && Maybe.map (Stack.beginSymbol >> Config.isVerbatimSymbol) (List.head tc.stack)
            == Just True
    then
        POP

    else if Config.notDelimiter Configuration.configuration Config.AllDelimiters firstChar then
        ADD

    else if value then
        PUSH { prefix = prefix, isMatch = isMatch }

    else if canPop configuration_ tc prefix_ then
        POP

    else
        COMMIT


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
