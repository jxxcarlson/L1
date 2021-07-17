module Parser.Print exposing (..)

import Library.Console as Console
import Library.Utility
import Parser.TextCursor exposing (StackItem, TextCursor, beginSymbol, content, precedingText, simplifyStack2)
import Render.Text


print : TextCursor -> String
print cursor =
    (printMessage cursor
        ++ printComplete cursor
        ++ printCursorText cursor
        ++ printPreceding cursor
        ++ printParsed cursor
        ++ printCaret
        ++ printRemaining cursor
        ++ printStack cursor.stack
        ++ (simplifyStack2 cursor.stack |> Console.blue |> Console.bgWhite)
    )
        |> Library.Utility.normalize
        |> String.replace "[ " "["
        |> String.trim


printMessage cursor =
    (String.fromInt cursor.count |> String.padLeft 2 '.')
        ++ (cursor.message |> String.padLeft 5 '.')
        ++ " :: "


printPreceding : TextCursor -> String
printPreceding cursor =
    case List.head cursor.stack of
        Nothing ->
            " " |> Console.blue |> Console.bgWhite

        Just stackTop ->
            " " ++ (List.filter (\s -> s /= "") (precedingText stackTop) |> String.join "") ++ " " |> Console.blue |> Console.bgWhite


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
        ++ String.trim (content item |> Maybe.withDefault "@NOTHING")


printStack : List StackItem -> String
printStack items =
    " " ++ (List.map printStackItem (List.reverse items) |> String.join " ") |> Console.bgMagenta |> Console.black


magenta : String -> String
magenta str =
    Console.magenta str


blue : String -> String
blue str =
    Console.black str |> Console.bgCyan
