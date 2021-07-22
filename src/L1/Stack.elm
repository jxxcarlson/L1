module L1.Stack exposing
    ( StackItem(..)
    , StackItemData
    , beginSymbol
    , endSymbol
    , etype
    , getContent
    , isNotReducibleWith
    , isReducible
    , isReducibleWith
    , isStrictlyReducible
    , show
    , showStack
    , simplifyStack
    , startPosition
    )

import L1.Check as Check
import L1.Config as Config
import L1.Loc exposing (Position)
import Library.Console as Console


type StackItem
    = Expect StackItemData
    | TextItem { content : String, position : Position }
    | EndMark { content : String, position : Position }


type alias StackItemData =
    { expect : Config.Expectation, content : String, count : Int, scanPoint : Int, position : Position }


isReducible : List StackItem -> Bool
isReducible stack =
    stack |> simplifyStack |> Check.reduces


isStrictlyReducible : List StackItem -> Bool
isStrictlyReducible stack =
    (simplifyStack >> Check.reduces2) stack == []


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

        EndMark { content } ->
            content


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

        EndMark { content } ->
            content


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


startPosition : StackItem -> Int
startPosition si =
    case si of
        Expect data ->
            data.position.start

        TextItem { position } ->
            position.start

        EndMark { position } ->
            position.start


beginSymbol : StackItem -> String
beginSymbol si =
    case si of
        Expect data ->
            data.expect.beginSymbol

        TextItem item ->
            "111"

        EndMark { content } ->
            content


endSymbol : StackItem -> Maybe String
endSymbol si =
    case si of
        Expect data ->
            data.expect.endSymbol

        TextItem _ ->
            Nothing

        EndMark { content } ->
            Just content


etype : StackItem -> Maybe Config.EType
etype si =
    case si of
        Expect data ->
            Just data.expect.etype

        _ ->
            Nothing


getContent : StackItem -> Maybe String
getContent si =
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

        EndMark { content } ->
            content
