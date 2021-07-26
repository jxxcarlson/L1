module L1.Parser.Print exposing (..)

import L1.Library.Console as Console
import L1.Library.Utility
import L1.Parser.Stack as Stack
import L1.Parser.TextCursor exposing (TextCursor)
import L1.Render.Text


print : TextCursor -> String
print cursor =
    printMessage cursor
        ++ printScanPoint cursor
        ++ printComplete cursor
        ++ printParsed cursor
        ++ printCaret
        ++ printRemaining cursor
        ++ printStack cursor.stack
        ++ printSimplifiedStack cursor.stack


printMessage cursor =
    (String.fromInt cursor.count |> String.padLeft 2 '.')
        ++ (cursor.message |> String.padLeft 7 '.')
        --++ " "
        --++ Debug.toString cursor.scannerType
        ++ " :: "
        |> Console.bgCyan
        |> Console.blue


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


printParsed cursor =
    cursor.parsed |> List.map L1.Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgCyan |> Console.black


printComplete cursor =
    cursor.complete |> List.map L1.Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgBlue


printStackItem : Stack.StackItem -> String
printStackItem item =
    case item of
        Stack.Expect _ ->
            Stack.beginSymbol item
                ++ String.trim (Stack.getContent item |> Maybe.withDefault "@NOTHING (3)")

        Stack.TextItem data ->
            data.content

        Stack.EndMark { content } ->
            content


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
