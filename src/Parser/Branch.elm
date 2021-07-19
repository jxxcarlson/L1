module Parser.Branch exposing (Operation(..), branch, canPop, canPush)

import Library.Console as Console
import Parser.Config as Config exposing (Configuration)
import Parser.Configuration as Configuration
import Parser.Stack as Stack
import Parser.TextCursor exposing (TextCursor)


type Operation
    = ADD
    | PUSH String
    | POP
    | COMMIT


branch : Config.Configuration -> TextCursor -> Char -> String -> Operation
branch configuration_ tc firstChar prefix_ =
    let
        { value, prefix } =
            canPush configuration_ tc prefix_ |> Debug.log (Console.magenta "CAN PUSH, (value, realPrefix)")
    in
    if Config.notDelimiter Configuration.configuration Config.AllDelimiters firstChar then
        ADD

    else if value then
        PUSH prefix

    else if canPop configuration_ tc prefix_ then
        POP

    else
        COMMIT


{-| The parser has paused at charater c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Configuration -> TextCursor -> String -> Bool
canPop configuration_ tc prefix =
    let
        _ =
            Debug.log (Console.cyan "canPop, isReducibleWith") ( prefix, tc.stack |> Stack.simplifyStack )
    in
    Stack.isReducibleWith prefix tc.stack |> Debug.log (Console.cyan "canPop")



--if canPopPrecondition configuration_ tc prefix then
--    case List.head tc.stack of
--        Nothing ->
--            False
--
--        Just stackTop ->
--            -- True
--            Maybe.map2 String.contains (endSymbol stackTop) (Just prefix) |> Maybe.withDefault False
--    -- TODO why should the above check be necessary?
--    -- With it, we get __many__ failing tests
--
--else
--    False


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


canPush : Configuration -> TextCursor -> String -> { value : Bool, prefix : String }
canPush configuration_ tc prefix =
    if Config.isVerbatimSymbol prefix then
        if Just prefix == (List.head tc.stack |> Maybe.map Stack.beginSymbol) then
            { value = False, prefix = prefix } |> Debug.log (Console.yellow "B 1")

        else
            { value = True, prefix = prefix } |> Debug.log (Console.yellow "B 2")

    else
        canPush1 configuration_ tc prefix |> Debug.log (Console.yellow "B 3")


canPush1 : Configuration -> TextCursor -> String -> { value : Bool, prefix : String }
canPush1 configuration_ tc prefix =
    (if prefix == "" then
        { value = False, prefix = "" }

     else if canPush2 configuration_ tc prefix then
        { value = True, prefix = prefix }

     else
        canPush configuration_ tc (String.dropLeft 1 prefix)
    )
        |> Debug.log (Console.cyan "canPush")


canPush2 configuration_ tc prefix =
    Config.isBeginSymbol configuration_ tc.scanPoint prefix
        || (Config.isEndSymbol configuration_ tc.scanPoint prefix
                && Stack.isNotReducibleWith prefix tc.stack
           )
