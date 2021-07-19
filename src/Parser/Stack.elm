module Parser.Stack exposing
    ( StackItem(..)
    , StackItemData
    , beginSymbol
    , content
    , endSymbol
    , etype
    , isNotReducibleWith
    , isReducible
    , isReducibleWith
    , show
    , showStack
    , simplifyStack
    )

import Library.Console as Console
import Parser.Check as Check
import Parser.Config as Config


type StackItem
    = Expect StackItemData
    | TextItem { content : String }
    | EndMark String


type alias StackItemData =
    { expect : Config.Expectation, content : String, count : Int, scanPoint : Int }


isReducible : List StackItem -> Bool
isReducible stack =
    stack |> simplifyStack |> Check.reduces


isReducibleWith : String -> List StackItem -> Bool
isReducibleWith str stack =
    ---stack |> simplifyStack |> (\st -> st ++ [ str ]) |> Debug.log (Console.yellow "SIMPLIFIED STACK") |> Check.reduces
    stack |> simplifyStack |> (\st -> str :: st) |> Debug.log (Console.yellow "SIMPLIFIED STACK") |> Check.reduces


isNotReducibleWith : String -> List StackItem -> Bool
isNotReducibleWith str stack =
    stack |> simplifyStack |> (\st -> st ++ [ str ]) |> Check.reduces |> not


simplifyStack : List StackItem -> List String
simplifyStack stack =
    List.map mark stack |> List.filter (\s -> s /= "000")


simplifyStack2 : List StackItem -> String
simplifyStack2 stack =
    String.join " " (simplifyStack stack)


show : StackItem -> String
show item =
    case item of
        Expect data ->
            data.expect.beginSymbol ++ data.content

        TextItem data ->
            data.content

        EndMark str ->
            str


showStack : List StackItem -> String
showStack stack =
    List.map show stack |> String.join " "


mark : StackItem -> String
mark stackItem =
    case stackItem of
        Expect data ->
            data.expect.beginSymbol

        TextItem i ->
            "000"

        EndMark str ->
            str


scanPoint : StackItem -> Int
scanPoint stackItem =
    case stackItem of
        Expect data ->
            data.scanPoint

        TextItem _ ->
            -- TODO: not a good idea
            -1

        EndMark _ ->
            -1


beginSymbol : StackItem -> String
beginSymbol si =
    case si of
        Expect data ->
            data.expect.beginSymbol

        TextItem item ->
            ""

        EndMark str ->
            str


endSymbol : StackItem -> Maybe String
endSymbol si =
    case si of
        Expect data ->
            data.expect.endSymbol

        TextItem _ ->
            Nothing

        EndMark str ->
            Just str


etype : StackItem -> Maybe Config.EType
etype si =
    case si of
        Expect data ->
            Just data.expect.etype

        _ ->
            Nothing


content : StackItem -> Maybe String
content si =
    case si of
        EndMark _ ->
            Nothing

        TextItem data ->
            Just data.content

        Expect data ->
            Just data.content


{-| for testing by humans
-}
simpleStackItem : StackItem -> String
simpleStackItem stackItem =
    case stackItem of
        Expect si ->
            "Offset " ++ String.fromInt si.scanPoint ++ ": " ++ si.content

        TextItem i ->
            i.content

        EndMark str ->
            str
