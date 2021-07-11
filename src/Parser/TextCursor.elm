module Parser.TextCursor exposing
    ( TextCursor, init
    , ErrorStatus(..), ParseError, ScannerType(..), add, canPop, commit, empty, parseResult, pop, print, push, simpleStackItem
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
    , message : String
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


type alias StackItem =
    { expect : Expectation, content : String, precedingText : List String, count : Int, offset : Int }


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
    , message = ""
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
    , message = "STAR"
    }


{-| for testing by humans
-}
simpleStackItem : StackItem -> String
simpleStackItem { content, offset } =
    "Offset " ++ String.fromInt offset ++ ": " ++ content


{-| Add text to the .text field
-}
add : String -> TextCursor -> TextCursor
add str tc =
    let
        ( stringToAdd, newStack ) =
            addContentToStack str tc.stack
    in
    { tc
        | count = tc.count + 1
        , text = stringToAdd ++ tc.text
        , stack = newStack
        , offset = tc.offset + String.length str
    }


addContentToStack : String -> List StackItem -> ( String, List StackItem )
addContentToStack str stack =
    case List.head stack of
        Nothing ->
            ( str, stack )

        Just top ->
            if top.content == "" then
                ( "", { top | content = str } :: List.drop 1 stack )

            else
                ( str, stack )


{-| A
-}
push : (String -> Element) -> Expectation -> TextCursor -> TextCursor
push parse expectation tc =
    { tc
        | count = tc.count + 1
        , offset = tc.offset + 1
        , stack =
            if tc.stack == [] then
                { expect = expectation, content = "", precedingText = [], count = tc.count, offset = tc.offset } :: tc.stack

            else
                { expect = expectation, content = tc.text, precedingText = [], count = tc.count, offset = tc.offset } :: tc.stack
        , parsed =
            if tc.stack == [] then
                []

            else
                -- parsed_ vvv NEW below
                tc.parsed
        , complete =
            if tc.stack == [] then
                if tc.text == "" then
                    []

                else
                    Text tc.text MetaData.dummy :: tc.parsed ++ tc.complete |> Debug.log (magenta "PUSH, complete (1)")

            else
                -- Debug.log (magenta "PUSH parsed") tc.parsed ++ Debug.log (magenta "PUSH complete (2)") tc.complete
                Debug.log (magenta "PUSH complete (2)") tc.complete
        , text = ""
    }


updateForPush : (String -> Element) -> TextCursor -> Expectation -> ( List Element, List StackItem )
updateForPush parse tc expectation =
    if tc.stack == [] then
        let
            complete =
                if tc.text == "" then
                    tc.parsed ++ tc.complete

                else
                    parse tc.text :: tc.parsed ++ tc.complete
        in
        ( complete, { expect = expectation, content = "", precedingText = [], count = tc.count, offset = tc.offset } :: tc.stack )

    else
        ( tc.parsed ++ tc.complete, { expect = expectation, content = tc.text, precedingText = [], count = tc.count, offset = tc.offset } :: tc.stack )


pop : (String -> Element) -> TextCursor -> TextCursor
pop parse tc =
    -- The cursors' offset is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
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
        _ =
            Debug.log (magenta "handleNonEmptyText") "!!!"

        parsed : List Element
        parsed =
            let
                parsed_ : List Element
                parsed_ =
                    getParsed parse stackTop tc |> Debug.log (magenta "handleNonEmptyText, parsed_")
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

                QuotedType ->
                    parsed_ |> Debug.log (magenta "QuotedType")

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

            -- , complete = parsed ++ tc.complete -- ++ List.map parse stackTop.precedingText
            , stack = stack
            , text = ""
            , scannerType = NormalScan
        }


getParsed : (String -> Element) -> StackItem -> TextCursor -> List Element
getParsed parse stackTop tc =
    ---- if stackTop.content == "" then
    --if True then
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



--
--else
--    let
--        _ =
--            Debug.log (magenta "getParsed BRANCH TWO, tc.text") tc.text
--
--        top =
--            case stackTop.expect.expectedEndChar of
--                Nothing ->
--                    String.fromChar stackTop.expect.beginChar
--                        ++ stackTop.content
--                        |> parse
--
--                Just endChar ->
--                    String.fromChar stackTop.expect.beginChar
--                        ++ stackTop.content
--                        ++ String.fromChar endChar
--                        |> parse
--
--        txt =
--            -- Text (tc.text ++ " ") MetaData.dummy
--            parse tc.text
--                |> Debug.log (magenta "getParsed, BRANCH TWO, txt")
--    in
--    [ AST.join top (List.reverse <| txt :: tc.parsed) ] |> Debug.log (magenta "getParsed, BRANCH 2")


handleEmptyText : (String -> Element) -> StackItem -> TextCursor -> TextCursor
handleEmptyText parse stackTop tc =
    let
        _ =
            Debug.log (magenta "handleEMPTYText") "!!!"
    in
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, offset = tc.offset + 1 }

        Just stackItem ->
            let
                _ =
                    Debug.log (magenta "ETYPE") stackItem.expect.etype

                ( fname, args_ ) =
                    stackItem.content
                        |> String.words
                        |> List.Extra.uncons
                        |> Maybe.withDefault ( "fname", [] )
                        |> Debug.log (magenta " ( fname, args_ ) ")

                args =
                    List.map (\a -> Text a MetaData.dummy) args_

                parsed =
                    case stackTop.expect.etype of
                        ElementType ->
                            handleFunction tc fname args

                        CodeType ->
                            [ Element (Name "code") (Text stackTop.content MetaData.dummy) MetaData.dummy ]

                        InlineMathType ->
                            [ Element (Name "math") (Text stackTop.content MetaData.dummy) MetaData.dummy ]

                        QuotedType ->
                            [ Text (Utility.Utility.unquote stackTop.content) MetaData.dummy ]
            in
            { tc
                | parsed = parsed

                --, complete = complete
                , stack = List.drop 1 tc.stack
                , offset = tc.offset + 1
                , count = tc.count + 1
                , text = ""
                , scannerType = NormalScan
            }


handleFunction : TextCursor -> String -> List Element -> List Element
handleFunction tc fname args =
    if fname == "" then
        let
            data =
                args ++ List.reverse tc.parsed
        in
        if data == [] then
            []

        else
            [ EList (args ++ List.reverse tc.parsed) MetaData.dummy ]

    else
        [ Element (AST.Name fname)
            (EList (args ++ List.reverse tc.parsed) MetaData.dummy)
            MetaData.dummy
        ]


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

        _ =
            Debug.log (magenta "commit_, parsed") (parsed |> List.map AST.simplify)

        complete =
            parsed ++ tc.complete
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                complete_ =
                    case top.expect.expectedEndChar of
                        Nothing ->
                            let
                                parsed_ =
                                    parsed ++ [ Text top.content MetaData.dummy ]
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



-- PREDICATES


canPop : TextCursor -> Char -> Bool
canPop tc c =
    Just c == (List.head tc.stack |> Maybe.andThen (.expect >> .expectedEndChar))



-- PRINT


print : TextCursor -> String
print cursor =
    (printMessage cursor
        ++ printComplete cursor
        ++ printStack cursor.stack
        ++ printCursorText cursor
        ++ printParsed cursor
        ++ printCaret
        ++ printRemaining cursor
    )
        |> Utility.Utility.normalize
        |> String.replace "[ " "["
        |> String.trim


printMessage cursor =
    (String.fromInt cursor.count |> String.padLeft 2 '.')
        ++ (cursor.message |> String.padLeft 5 '.')
        ++ " :: "
        ++ (Debug.toString cursor.scannerType |> String.padLeft 12 '.')
        ++ " :: "


printCaret =
    " ^ " |> Console.bgRed


printRemaining cursor =
    String.dropLeft cursor.offset cursor.remainingSource |> Console.black |> Console.bgGreen


printCursorText cursor =
    cursor.text ++ " " |> Console.black |> Console.bgYellow


printParsed cursor =
    cursor.parsed |> List.map Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgCyan |> Console.black


printComplete cursor =
    cursor.complete |> List.map Render.Text.print |> String.join " " |> (\x -> x ++ " ") |> Console.bgBlue


printStackItem : StackItem -> String
printStackItem item =
    String.fromChar item.expect.beginChar
        ++ String.trim item.content


printStack : List StackItem -> String
printStack items =
    List.map printStackItem (List.reverse items) |> String.join " " |> String.trim |> Console.bgMagenta |> Console.black


magenta : String -> String
magenta str =
    Console.magenta str
