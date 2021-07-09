module Parser.TextCursor exposing
    ( TextCursor, init
    , ErrorStatus(..), ParseError, add, commit, empty, parseResult, pop, push, simpleStackItem
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Html exposing (a)
import List.Extra
import Parser.AST as AST exposing (Element(..), simplify)
import Parser.MetaData exposing (MetaData)
import Parser.Parser as Parser


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
    }


type alias StackItem =
    { expect : Expectation, data : String, count : Int, offset : Int }


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
    }


{-| for testing by humans
-}
simpleStackItem : StackItem -> String
simpleStackItem { data, offset } =
    "Offset " ++ String.fromInt offset ++ ": " ++ data


type alias Expectation =
    { begin : Char, end : Char }


{-| Add text to the .text field
-}
add : String -> TextCursor -> TextCursor
add str tc =
    let
        _ =
            Debug.log "!" "ADD"
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
            Debug.log "!" "PUSH"
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

                _ =
                    tc.text |> Debug.log "TXT"
            in
            { tc
                | count = tc.count + 1
                , offset = tc.offset + 1
                , stack = { expect = expectation, data = "", count = tc.count, offset = tc.offset } :: tc.stack
                , parsed = []
                , complete = complete |> Debug.log "PUSH, COMPLETE"
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
                , stack = { expect = expectation, data = "", count = tc.count, offset = tc.offset } :: top_ :: rest
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
            Debug.log "!" "POP"
    in
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, offset = tc.offset + 1 }

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
        top =
            if stackTop.data == "" then
                Raw (tc.text ++ " ") Parser.MetaData.dummy

            else
                String.fromChar stackTop.expect.begin
                    ++ stackTop.data
                    ++ String.fromChar stackTop.expect.end
                    |> parse

        txt =
            if stackTop.data == "" then
                String.fromChar stackTop.expect.begin
                    ++ tc.text
                    ++ String.fromChar stackTop.expect.end
                    |> parse

            else
                Raw (tc.text ++ " ") Parser.MetaData.dummy

        parsed =
            if stackTop.data == "" then
                txt :: tc.parsed

            else
                [ AST.join top (List.reverse <| txt :: tc.parsed) ]

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
        }

    else
        { tc
            | offset = tc.offset + 1
            , count = tc.count + 1
            , parsed = parsed
            , stack = stack
            , text = ""
        }


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
                    List.map (\a -> Raw (a ++ " ") Parser.MetaData.dummy) args_

                newParsed =
                    Element (AST.Name fname)
                        []
                        (EList (args ++ List.reverse tc.parsed) Parser.MetaData.dummy)
                        Parser.MetaData.dummy

                _ =
                    newParsed |> AST.simplify |> Debug.log "NEW PARSED"

                _ =
                    args ++ List.reverse tc.parsed |> List.map AST.simplify |> Debug.log "args ++ reversed from tc"
            in
            { tc
                | parsed = [ newParsed ]

                --, complete = newParsed :: tc.complete
                , stack = List.drop 1 tc.stack
                , offset = tc.offset + 1
                , count = tc.count + 1
                , text = ""
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
                AST.Raw tc.text Parser.MetaData.dummy :: tc.parsed

        complete =
            parsed ++ tc.complete

        _ =
            complete |> List.map simplify |> Debug.log "!! COMPLETE"
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                _ =
                    Debug.log "AT END, STACK SIZE" (1 + List.length restOfStack)

                _ =
                    Debug.log "TOP OF STACK" top

                _ =
                    Debug.log "PARSED" (tc.parsed |> List.map AST.simplify)

                newParsed =
                    Parser.parse tc.generation <| "[" ++ String.trim top.data ++ "]"

                --- more here
                errorMessage =
                    StackError top.offset tc.offset "((unmatched bracket))" (String.slice top.offset tc.offset tc.source)

                trailingElement =
                    Parser.parse tc.generation tc.text
            in
            commit
                { tc
                    | count = 1 + tc.count
                    , text = ""
                    , stack = restOfStack
                    , parsed = []
                    , complete =
                        List.reverse tc.complete
                            ++ [ errorMessage ]
                }
