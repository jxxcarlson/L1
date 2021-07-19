module Parser.Print exposing (..)

import Library.Console as Console
import Library.Utility
import Parser.Stack as Stack
import Parser.TextCursor exposing (TextCursor)
import Render.Text


print : TextCursor -> String
print cursor =
    printMessage cursor
        ++ printScanPoint cursor
        ++ printComplete cursor
        ++ printCursorText cursor
        ++ printParsed cursor
        ++ printCaret
        ++ printRemaining cursor
        ++ printStack cursor.stack
        ++ printSimplifiedStack cursor.stack



--|> Library.Utility.normalize
-- |> String.replace "[ " "["
-- |> String.trim


printMessage cursor =
    (String.fromInt cursor.count |> String.padLeft 2 '.')
        ++ (cursor.message |> String.padLeft 5 '.')
        ++ " :: "
        |> Console.bgBlue
        |> Console.red


printScanPoint cursor =
    let
        firstChar =
            String.slice cursor.scanPoint (cursor.scanPoint + 1) cursor.source
    in
    String.fromInt cursor.scanPoint ++ firstChar |> String.padRight 5 '.' |> Console.magenta


printCaret =
    " ^ " |> Console.bgRed


printRemaining cursor =
    String.dropLeft cursor.scanPoint cursor.source ++ " " |> Console.black |> Console.bgGreen


printCursorText cursor =
    cursor.text ++ " " |> Console.black |> Console.bgYellow


printParsed cursor =
    cursor.parsed |> List.map Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgCyan |> Console.black


printComplete cursor =
    cursor.complete |> List.map Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgBlue


printStackItem : Stack.StackItem -> String
printStackItem item =
    case item of
        Stack.Expect _ ->
            Stack.beginSymbol item
                ++ String.trim (Stack.content item |> Maybe.withDefault "@NOTHING (3)")

        Stack.TextItem data ->
            data.content

        Stack.EndMark str ->
            str


enclose delimiter str =
    delimiter ++ str ++ delimiter


printStack : List Stack.StackItem -> String
printStack items =
    " " ++ ((List.map printStackItem (List.reverse items) |> String.join " ") |> Console.bgMagenta |> Console.black) ++ " "


printSimplifiedStack : List Stack.StackItem -> String
printSimplifiedStack items =
    " " ++ (items |> List.reverse |> Stack.simplifyStack |> String.join "" |> Console.bgCyan |> Console.black) ++ " "


magenta : String -> String
magenta str =
    Console.magenta str


blue : String -> String
blue str =
    Console.black str |> Console.bgCyan
