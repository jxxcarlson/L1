module Parser.TextCursor exposing
    ( TextCursor, init
    , ErrorStatus(..), ParseError, ScannerType(..), add, commit, empty, parseResult, pop, print, push, simpleStackItem
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

-- import Render.Text

import List.Extra
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Config exposing (EType(..), Expectation)
import Parser.MetaData as MetaData exposing (MetaData)
import Parser.Utility
import Render.Text
import Utility.Console as Console
import Utility.Utility


{-| TODO: give an account of what these fields do
-}
type alias TextCursor =
    { count : Int
    , generation : Int
    , offset : Int
    , length : Int

    --
    , source : String
    , remainingSource : String
    , text : String
    , parsed : List Element
    , complete : List Element
    , stack : List StackItem
    , scannerType : ScannerType
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


type alias StackItem =
    { expect : Expectation, data : String, preceding : List String, count : Int, offset : Int }


type alias ParseError =
    { status : ErrorStatus, correctedText : List String }


type ErrorStatus
    = NoError
    | PipeError
    | RightBracketError
    | LeftBracketError
    | UnhandledError


parseResult : TextCursor -> List Element
parseResult t =
    t.parsed


empty : TextCursor
empty =
    { count = 0
    , generation = 0
    , offset = 0
    , length = 0

    --
    , source = ""
    , remainingSource = ""
    , text = ""
    , parsed = []
    , complete = []
    , stack = []
    , scannerType = NormalScan
    }


{-| initialize with source text
-}
init : Int -> String -> TextCursor
init generation source =
    { count = 0
    , generation = generation
    , offset = 0
    , length = String.length source

    --
    , source = source
    , remainingSource = source
    , text = ""
    , parsed = []
    , complete = []
    , stack = []
    , scannerType = NormalScan
    }


{-| for testing by humans
-}
simpleStackItem : StackItem -> String
simpleStackItem { data, offset } =
    "Offset " ++ String.fromInt offset ++ ": " ++ data


{-| Add text to the .text field
-}
add : String -> TextCursor -> TextCursor
add str tc =
    let
        _ =
            Debug.log (String.fromInt tc.count) "ADD ----------------------------------------"
    in
    { tc
        | count = tc.count + 1
        , text = str ++ tc.text
        , offset = tc.offset + String.length str
    }


{-| A
-}
push : (String -> Element) -> Expectation -> TextCursor -> TextCursor
push parse expectation tc =
    let
        _ =
            Debug.log (String.fromInt tc.count) "PUSH ----------------------------------------"
    in
    case tc.stack of
        [] ->
            -- The stack is empty, so we prepare for a new element:
            -- (a) parse tc.text, prepend it to tc.parsed and tc.complete
            -- (b) clear tc.text and tc.text
            -- (c) push a stackItem onto the stack, recording the start
            --     character, the expected end character if any, and
            --     the string data, which in this case is empty
            -- (d) increment the offset
            let
                complete =
                    if tc.text /= "" then
                        parse tc.text :: tc.parsed ++ tc.complete

                    else
                        tc.parsed ++ tc.complete
            in
            { tc
                | count = tc.count + 1
                , offset = tc.offset + 1
                , stack = { expect = expectation, data = "", preceding = tc.text :: [], count = tc.count, offset = tc.offset } :: tc.stack
                , parsed = []
                , complete = complete
                , text = ""
            }

        top :: rest ->
            -- The stack has at least one element, 'top'
            -- (a) if the cursor holds text, put in the data field of 'top'
            -- (b) set the text field to empty
            -- (c) push an empty stackItem
            -- (d) increment the offset
            let
                top_ =
                    if String.trim tc.text == "" then
                        top

                    else
                        { top | data = tc.text }
            in
            { tc
                | count = tc.count + 1
                , offset = tc.offset + 1
                , stack = { expect = expectation, data = "", preceding = tc.text :: top.preceding, count = tc.count, offset = tc.offset } :: top_ :: rest
                , text = ""
            }


pop : (String -> Element) -> TextCursor -> TextCursor
pop parse tc =
    -- The cursors' offset is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    let
        _ =
            Debug.log (String.fromInt tc.count) "POP ----------------------------------------"
    in
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, offset = tc.offset + 1, scannerType = NormalScan }

        Just stackTop ->
            if tc.text /= "" then
                handleNonEmptyText parse stackTop tc

            else
                handleEmptyText parse stackTop tc


handleNonEmptyText : (String -> Element) -> StackItem -> TextCursor -> TextCursor
handleNonEmptyText parse stackTop tc =
    -- cursor.text is nonempty.  We proceed as follows, where 'stackTop'
    -- is the item on the top of the stack.
    -- (a)
    let
        parsed =
            let
                parsed_ : List Element
                parsed_ =
                    getParsed parse stackTop tc
            in
            case stackTop.expect.etype of
                InlineMathType ->
                    Element
                        (Name "math")
                        (EList (List.map (Parser.Utility.mapRaw Utility.Utility.clipEnds) parsed_) MetaData.dummy)
                        MetaData.dummy
                        |> (\x -> [ x ])

                CodeType ->
                    Element
                        (Name "code")
                        (EList (List.map (Parser.Utility.mapRaw Utility.Utility.clipEnds) parsed_) MetaData.dummy)
                        MetaData.dummy
                        |> (\x -> [ x ])

                _ ->
                    parsed_

        stack =
            List.drop 1 tc.stack
    in
    if stack == [] then
        { tc
            | offset = tc.offset + 1
            , count = tc.count + 1
            , parsed = []
            , stack = []
            , complete = parsed ++ tc.complete
            , text = ""
            , scannerType = NormalScan
        }

    else
        { tc
            | offset = tc.offset + 1
            , count = tc.count + 1
            , parsed = parsed
            , stack = stack
            , text = ""
            , scannerType = NormalScan
        }


getParsed : (String -> Element) -> StackItem -> TextCursor -> List Element
getParsed parse stackTop tc =
    if stackTop.data == "" then
        let
            txt =
                case stackTop.expect.expectedEndChar of
                    Nothing ->
                        String.fromChar stackTop.expect.beginChar
                            ++ tc.text
                            |> parse

                    Just endChar ->
                        String.fromChar stackTop.expect.beginChar
                            ++ tc.text
                            ++ String.fromChar endChar
                            |> Utility.Utility.ifApply (tc.scannerType == NormalScan) parse Parser.Utility.makeRaw
        in
        txt :: tc.parsed

    else
        let
            _ =
                Debug.log "BRANCH" 2

            top =
                case stackTop.expect.expectedEndChar of
                    Nothing ->
                        String.fromChar stackTop.expect.beginChar
                            ++ stackTop.data
                            |> parse

                    Just endChar ->
                        String.fromChar stackTop.expect.beginChar
                            ++ stackTop.data
                            ++ String.fromChar endChar
                            |> parse

            txt =
                Text (tc.text ++ " ") MetaData.dummy
        in
        [ AST.join top (List.reverse <| txt :: tc.parsed) ]


handleEmptyText : (String -> Element) -> StackItem -> TextCursor -> TextCursor
handleEmptyText parse stackTop tc =
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, offset = tc.offset + 1 }

        Just stackItem ->
            let
                ( fname, args_ ) =
                    stackItem.data |> String.words |> List.Extra.uncons |> Maybe.withDefault ( "fname", [] )

                args =
                    List.map (\a -> Text (a ++ " ") MetaData.dummy) args_

                newParsed =
                    Element (AST.Name fname)
                        (EList (args ++ List.reverse tc.parsed) MetaData.dummy)
                        MetaData.dummy
            in
            { tc
                | parsed = [ newParsed ]
                , stack = List.drop 1 tc.stack
                , offset = tc.offset + 1
                , count = tc.count + 1
                , text = ""
                , scannerType = NormalScan
            }


commit : TextCursor -> TextCursor
commit tc =
    tc |> commit_ |> (\tc2 -> { tc2 | complete = List.reverse tc2.complete })


commit_ : TextCursor -> TextCursor
commit_ tc =
    let
        parsed =
            if tc.text == "" then
                tc.parsed

            else
                AST.Text tc.text MetaData.dummy :: tc.parsed

        complete =
            parsed ++ tc.complete
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                _ =
                    Debug.log "TOP OF STACK AT END" top

                _ =
                    Debug.log "@@ parsed" parsed

                _ =
                    Debug.log "@@ top.data" top.data

                _ =
                    Debug.log "@@ complete  " complete

                complete_ =
                    case top.expect.expectedEndChar of
                        Nothing ->
                            let
                                parsed_ =
                                    parsed ++ [ Text top.data MetaData.dummy ]
                            in
                            List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]

                        -- Element (AST.Name "heading") (EList parsed MetaData.dummy) MetaData.dummy :: List.reverse tc.complete
                        Just _ ->
                            let
                                errorMessage =
                                    StackError top.offset tc.offset "((unmatched bracket))" (String.slice top.offset tc.offset tc.source)
                            in
                            List.reverse tc.complete ++ [ errorMessage ]
            in
            commit
                { tc
                    | count = 1 + tc.count
                    , text = ""
                    , stack = restOfStack
                    , parsed = []
                    , complete = complete_
                }



-- PRINT


print : TextCursor -> String
print cursor =
    (case List.head cursor.stack of
        Nothing ->
            printComplete cursor
                ++ printStack cursor.stack
                ++ printCursorText cursor
                ++ printParsed cursor
                ++ printCaret
                ++ printRemaining cursor

        Just top ->
            printComplete cursor
                ++ printStack cursor.stack
                ++ printCursorText cursor
                --++ String.join " " top.preceding
                --++ " "
                ++ printParsed cursor
                ++ printCaret
                ++ printRemaining cursor
    )
        |> Utility.Utility.normalize
        |> String.replace "[ " "["
        |> String.trim


printCaret =
    " ^ " |> Console.bgRed


printRemaining cursor =
    String.dropLeft cursor.offset cursor.remainingSource |> Console.black |> Console.bgGreen


printCursorText cursor =
    cursor.text ++ " " |> Console.black |> Console.bgYellow


printParsed cursor =
    cursor.parsed |> List.map Render.Text.print |> String.join " " |> Console.bgMagenta


printComplete cursor =
    cursor.parsed |> List.map Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgBlue


printStackItem : StackItem -> String
printStackItem item =
    String.fromChar item.expect.beginChar
        ++ String.trim item.data


printStack : List StackItem -> String
printStack items =
    List.map printStackItem (List.reverse items) |> String.join " " |> String.trim |> Console.bgWhite |> Console.black
