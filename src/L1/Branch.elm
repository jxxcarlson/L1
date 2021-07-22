module L1.Branch exposing (Operation(..), branch)

import L1.Config as Config exposing (Configuration)
import L1.Configuration as Configuration
import L1.Stack as Stack
import L1.TextCursor exposing (TextCursor)
import Library.Console as Console


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



--|> Debug.log (Console.cyan "BRANCH")


{-| The parser has paused at character c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Configuration -> TextCursor -> String -> Bool
canPop configuration_ tc prefix =
    let
        _ =
            Debug.log (Console.cyan "canPop, isReducibleWith") ( prefix, tc.stack |> Stack.simplifyStack )
    in
    Stack.isReducibleWith prefix tc.stack |> Debug.log (Console.cyan "canPop")


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
    if Config.isVerbatimSymbol prefix then
        if Just prefix == (List.head tc.stack |> Maybe.map Stack.beginSymbol) then
            if Config.isVerbatimSymbol prefix then
                { value = True, prefix = prefix, isMatch = True }

            else
                -- TODO: think about the above
                { value = False, prefix = prefix, isMatch = False }

        else
            { value = True, prefix = prefix, isMatch = False }

    else
        canPush1 configuration_ tc prefix


canPush1 : Configuration -> TextCursor -> String -> { value : Bool, prefix : String, isMatch : Bool }
canPush1 configuration_ tc prefix =
    if prefix == "" then
        { value = False, prefix = "", isMatch = False }

    else if canPush2 configuration_ tc prefix then
        { value = True, prefix = prefix, isMatch = False }

    else
        canPush configuration_ tc (String.dropLeft 1 prefix)


canPush2 : Configuration -> { a | scanPoint : Int, stack : List Stack.StackItem } -> String -> Bool
canPush2 configuration_ tc prefix =
    Config.isBeginSymbol configuration_ tc.scanPoint prefix
        || (Config.isEndSymbol configuration_ tc.scanPoint prefix
                && Stack.isNotReducibleWith prefix tc.stack
           )
