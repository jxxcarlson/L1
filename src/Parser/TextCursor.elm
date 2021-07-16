module Parser.TextCursor exposing
    ( TextCursor, init
    ,  ScannerType(..)
      , StackItem
      , add
      , canPop
      , canPush
      , commit
      , isBalanced
      , pop
      , push
      , simpleStackItem
        --, ErrorStatus(..)

    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Library.Utility
import List.Extra
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Config exposing (EType(..), Expectation)
import Parser.MetaData as MetaData exposing (MetaData)


{-| TODO: give an account of what these fields do
-}
type alias TextCursor =
    { count : Int
    , generation : Int
    , scanPoint : Int
    , length : Int

    ---
    , source : String
    , text : String
    , parsed : List Element -- might be incorporated later
    , complete : List Element -- no changes will be made
    , stack : List StackItem -- a stack of unclosed elements
    , scannerType : ScannerType

    ---
    , message : String
    }


type ScannerType
    = NormalScan
    | VerbatimScan Char


type alias StackItem =
    { expect : Expectation, content : String, precedingText : List String, count : Int, scanPoint : Int }


{-| initialize with source text
-}
init : Int -> String -> TextCursor
init generation source =
    { count = 0
    , generation = generation

    --
    , scanPoint = 0
    , length = String.length source
    , scannerType = NormalScan

    --
    , source = source
    , text = ""
    , parsed = []
    , complete = []
    , stack = []

    --
    , message = "STAR"
    }


{-| for testing by humans
-}
simpleStackItem : StackItem -> String
simpleStackItem { content, scanPoint } =
    "Offset " ++ String.fromInt scanPoint ++ ": " ++ content


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


{-| Used by add
-}
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
push : Expectation -> TextCursor -> TextCursor
push expectation tc =
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
                            [ Text (Library.Utility.unquote stackTop.content) MetaData.dummy ] ++ tc.parsed
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
        ++ [ Element (Name "error") (Text (" unmatched " ++ top.expect.beginSymbol ++ " ") MetaData.dummy) MetaData.dummy
           , Text top.content MetaData.dummy
           ]
        ++ List.reverse tc.parsed
        ++ [ Text tc.text MetaData.dummy ]



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


isBalanced : String -> Bool
isBalanced str =
    (List.length <| String.indices "[" str) == (List.length <| String.indices "]" str)



-- PRINT
