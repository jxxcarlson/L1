module Parser.TextCursor exposing
    ( TextCursor, init
    , ErrorStatus(..), ParseError, ScannerType(..), add, canPop, canPush, commit, empty, parseResult, pop, print, push, simpleStackItem
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
    , scanPoint : Int
    , length : Int

    --
    , source : String
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
    { expect : Expectation, content : String, precedingText : List String, count : Int, scanPoint : Int }


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
    , scanPoint = 0
    , length = 0

    --
    , source = ""
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
    , scanPoint = 0
    , length = String.length source

    --
    , source = source
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
simpleStackItem { content, scanPoint } =
    "Offset " ++ String.fromInt scanPoint ++ ": " ++ content


{-| Add text to the .text field
-}
add1 : String -> TextCursor -> TextCursor
add1 str tc =
    { tc
        | count = tc.count + 1
        , text =
            case List.head tc.stack of
                Nothing ->
                    str ++ tc.text

                Just top ->
                    if top.content == "" then
                        tc.text

                    else
                        str ++ tc.text
        , stack =
            case List.head tc.stack of
                Nothing ->
                    tc.stack

                Just top ->
                    if top.content == "" then
                        { top | content = str } :: List.drop 1 tc.stack

                    else
                        tc.stack
        , scanPoint = tc.scanPoint + String.length str
    }



---- XXXX


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
        , scanPoint = tc.scanPoint + String.length str
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
        , scanPoint = tc.scanPoint + 1
        , stack =
            case List.head tc.stack of
                Nothing ->
                    { expect = expectation, content = "", precedingText = [], count = tc.count, scanPoint = tc.scanPoint } :: tc.stack

                Just stackTop ->
                    { expect = expectation, content = "", precedingText = [ tc.text ], count = tc.count, scanPoint = tc.scanPoint } :: tc.stack
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
                    Text tc.text MetaData.dummy :: tc.parsed ++ tc.complete

            else
                tc.complete
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
        ( complete, { expect = expectation, content = "", precedingText = [], count = tc.count, scanPoint = tc.scanPoint } :: tc.stack )

    else
        ( tc.parsed ++ tc.complete, { expect = expectation, content = tc.text, precedingText = [], count = tc.count, scanPoint = tc.scanPoint } :: tc.stack )


pop : (String -> Element) -> TextCursor -> TextCursor
pop parse tc =
    -- The cursors' scanPoint is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, scanPoint = tc.scanPoint + 1, scannerType = NormalScan }

        Just stackTop ->
            handleText parse stackTop tc


handleText : (String -> Element) -> StackItem -> TextCursor -> TextCursor
handleText parse stackTop tc =
    case List.head tc.stack of
        Nothing ->
            { tc | count = tc.count + 1, scanPoint = tc.scanPoint + 1 }

        Just stackTop_ ->
            let
                ( fname, args_ ) =
                    stackTop_.content
                        |> String.words
                        |> List.Extra.uncons
                        |> Maybe.withDefault ( "fname", [] )

                args =
                    List.map (\a -> Text a MetaData.dummy) args_

                parsed : List Element
                parsed =
                    case stackTop.expect.etype of
                        ElementType ->
                            let
                                new =
                                    handleFunction parse tc stackTop_ fname args
                            in
                            case ( List.head new, tc.text ) of
                                ( Nothing, _ ) ->
                                    new

                                ( _, "" ) ->
                                    new

                                ( Just first_, text ) ->
                                    [ AST.join first_ (List.drop 1 new ++ [ parse text ]) ]

                        CodeType ->
                            [ Element (Name "code") (Text stackTop.content MetaData.dummy) MetaData.dummy ] ++ tc.parsed

                        InlineMathType ->
                            [ Element (Name "math2") (Text stackTop.content MetaData.dummy) MetaData.dummy ] ++ tc.parsed

                        QuotedType ->
                            [ Text (Utility.Utility.unquote stackTop.content) MetaData.dummy ] ++ tc.parsed
            in
            { tc
                | parsed = parsed

                --, complete = complete
                , stack = List.drop 1 tc.stack
                , scanPoint = tc.scanPoint + 1
                , count = tc.count + 1
                , text = ""
                , scannerType = NormalScan
            }


getParsed : (String -> Element) -> StackItem -> TextCursor -> List Element
getParsed parse stackTop tc =
    ---- if stackTop.content == "" then
    --if True then
    let
        txt =
            case stackTop.expect.endSymbol of
                Nothing ->
                    stackTop.expect.beginSymbol
                        ++ tc.text
                        |> parse

                Just endSymbol ->
                    stackTop.expect.beginSymbol
                        ++ tc.text
                        ++ endSymbol
                        |> Utility.Utility.ifApply (tc.scannerType == NormalScan) parse Parser.Utility.makeText
    in
    txt :: tc.parsed


handleFunction : (String -> Element) -> TextCursor -> StackItem -> String -> List Element -> List Element
handleFunction parse tc stackTop fname args =
    if fname == "" then
        let
            data =
                args ++ List.reverse tc.parsed
        in
        if data == [] then
            []

        else
            [ EList (args ++ List.reverse tc.parsed) MetaData.dummy ]

    else if args == [] then
        [ Element (AST.Name fname)
            (EList (List.reverse tc.parsed) MetaData.dummy)
            MetaData.dummy
        ]

    else if stackTop.precedingText /= [] then
        [ Element (AST.Name fname) (EList args MetaData.dummy) MetaData.dummy ]
            ++ List.map parse (List.filter (\s -> s /= "") stackTop.precedingText)
            ++ tc.parsed

    else
        [ AST.join (Element (AST.Name fname) (EList args MetaData.dummy) MetaData.dummy) tc.parsed ]


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
                complete_ =
                    case top.expect.endSymbol of
                        Nothing ->
                            let
                                parsed_ =
                                    parsed ++ [ Text top.content MetaData.dummy ]
                            in
                            if String.left 1 top.expect.beginSymbol == "#" then
                                handleHeadings tc top parsed_

                            else if top.expect.beginSymbol == ":" then
                                handleLineCommand tc parsed_

                            else
                                let
                                    errorMessage =
                                        StackError top.scanPoint tc.scanPoint ("((unknown delimiter " ++ top.expect.beginSymbol ++ " at position " ++ String.fromInt top.scanPoint ++ "))") (String.slice top.scanPoint tc.scanPoint tc.source)
                                in
                                List.reverse tc.complete ++ [ errorMessage ]

                        Just _ ->
                            handleError tc top
            in
            commit
                { tc
                    | count = 1 + tc.count
                    , text = ""
                    , stack = restOfStack
                    , parsed = []
                    , complete = complete_
                }


handleHeadings tc top parsed_ =
    if top.expect.beginSymbol == "#" then
        List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]

    else if top.expect.beginSymbol == "##" then
        List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]

    else if top.expect.beginSymbol == "###" then
        List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]

    else
        List.reverse tc.complete ++ [ Element (AST.Name "heading") (EList (List.reverse parsed_) MetaData.dummy) MetaData.dummy ]


handleLineCommand tc parsed_ =
    case List.Extra.uncons (List.reverse parsed_) of
        Nothing ->
            List.reverse tc.complete ++ [ Text "empty" MetaData.dummy ]

        Just ( first, [] ) ->
            case String.words (AST.getText first) of
                [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name "empty") (EList [] MetaData.dummy) MetaData.dummy ]

                name :: [] ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) (EList [] MetaData.dummy) MetaData.dummy ]

                name :: rest ->
                    List.reverse tc.complete ++ [ Element (AST.Name (String.trim name)) (Text (String.join " " rest) MetaData.dummy) MetaData.dummy ]

        Just ( first, rest ) ->
            List.reverse tc.complete ++ [ Element (AST.Name (String.trim (AST.getText first))) (EList rest MetaData.dummy) MetaData.dummy ]


handleError : TextCursor -> StackItem -> List Element
handleError tc top =
    List.reverse tc.complete
        ++ [ Element (Name "error") (Text ("unmatched: " ++ top.expect.beginSymbol ++ " ") MetaData.dummy) MetaData.dummy
           , Text top.content MetaData.dummy
           ]
        ++ tc.parsed
        ++ [ Text tc.text MetaData.dummy ]



-- HELPERS


dropFirstWord : Int -> String -> String
dropFirstWord k str =
    if k == 0 then
        Utility.Utility.dropWords 1 str

    else
        str



-- PREDICATES


{-| The parser has paused at charater c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Parser.Config.Configuration -> TextCursor -> String -> Bool
canPop configuration tc prefix =
    if canPopPrecondition configuration tc prefix then
        case List.head tc.stack of
            Nothing ->
                False

            Just stackTop ->
                -- True
                Maybe.map2 String.contains stackTop.expect.endSymbol (Just prefix) |> Maybe.withDefault False
        -- TODO why should the above check be necessary?
        -- With it, we get __many__ failing tests

    else
        False


canPopPrecondition : Parser.Config.Configuration -> TextCursor -> String -> Bool
canPopPrecondition configuration tc prefix =
    let
        isEndSymbol =
            Parser.Config.isEndSymbol configuration tc.scanPoint prefix
    in
    if isEndSymbol then
        True

    else if String.length prefix > 1 then
        canPopPrecondition configuration tc (String.dropLeft 1 prefix)

    else
        False


canPush : Parser.Config.Configuration -> TextCursor -> String -> Bool
canPush configuration tc prefix =
    Parser.Config.isBeginSymbol configuration tc.scanPoint prefix



-- PRINT


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


printPreceding : TextCursor -> String
printPreceding cursor =
    case List.head cursor.stack of
        Nothing ->
            " " |> Console.blue |> Console.bgWhite

        Just stackTop ->
            " " ++ (List.filter (\s -> s /= "") stackTop.precedingText |> String.join "") ++ " " |> Console.blue |> Console.bgWhite


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
    item.expect.beginSymbol
        ++ String.trim item.content


printStack : List StackItem -> String
printStack items =
    " " ++ (List.map printStackItem (List.reverse items) |> String.join " ") |> Console.bgMagenta |> Console.black


magenta : String -> String
magenta str =
    Console.magenta str


blue : String -> String
blue str =
    Console.black str |> Console.bgCyan
