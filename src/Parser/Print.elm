module Parser.Print exposing (..)

import Library.Console as Console
import Library.Utility
import Parser.TextCursor exposing (StackItem, TextCursor, beginSymbol, content, simplifyStack2)
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



--|> Library.Utility.normalize
-- |> String.replace "[ " "["
-- |> String.trim


printMessage cursor =
    (String.fromInt cursor.count |> String.padLeft 2 '.')
        ++ (cursor.message |> String.padLeft 5 '.')
        ++ " :: "


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


printStackItem : StackItem -> String
printStackItem item =
    beginSymbol item
        ++ String.trim (content item |> Maybe.withDefault "@NOTHING (3)")


enclose delimiter str =
    delimiter ++ str ++ delimiter


printStack : List StackItem -> String
printStack items =
    " " ++ ((List.map printStackItem (List.reverse items) |> String.join " ") |> Console.bgMagenta |> Console.black) ++ " "


magenta : String -> String
magenta str =
    Console.magenta str


blue : String -> String
blue str =
    Console.black str |> Console.bgCyan
