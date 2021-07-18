module Parser.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), StackItem(..), add, advance, beginSymbol, canPop, canPush, commit, configuration, content, pop, push, simpleStackItem, simplifyStack, simplifyStack2
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Library.Console as Console
import Library.ParserTools as ParserTools
import Library.Utility
import List.Extra
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced
import Parser.Check as Check
import Parser.Config as Config exposing (Configuration, EType(..), Expectation)
import Parser.Configuration as Configuration
import Parser.MetaData as MetaData exposing (MetaData)


isReducible : List StackItem -> Bool
isReducible stack =
    stack |> simplifyStack |> Check.reduces


isReducibleWith : String -> List StackItem -> Bool
isReducibleWith str stack =
    stack |> simplifyStack |> (\st -> st ++ [ str ]) |> Check.reduces


isNotReducibleWith : String -> List StackItem -> Bool
isNotReducibleWith str stack =
    stack |> simplifyStack |> (\st -> st ++ [ str ]) |> Check.reduces |> not


simplifyStack : List StackItem -> List String
simplifyStack stack =
    List.map mark stack |> List.filter (\s -> s /= "000")


simplifyStack2 : List StackItem -> String
simplifyStack2 stack =
    String.join " " (simplifyStack stack)


configuration =
    Config.configure Configuration.expectations


advance : TextCursor -> String -> ParserTools.StringData
advance cursor textToProcess =
    case cursor.scannerType of
        NormalScan ->
            advanceNormal configuration cursor.scanPoint textToProcess

        VerbatimScan c ->
            advanceVerbatim c textToProcess


{-| Return the longest prefix of str that does not contain a delimiter.
The delimiter sets used depend upon position. One set for position = 0,
another for position /= 0.
-}
advanceNormal : Configuration -> Int -> String -> ParserTools.StringData
advanceNormal config position str =
    case Parser.Advanced.run (ParserTools.text (Config.notDelimiter configuration position) (Config.notDelimiter configuration position)) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


{-| Advance, but according to different criteria, because the
scanner type has been set to 'VerbatimScan c'
-}
advanceVerbatim : Char -> String -> ParserTools.StringData
advanceVerbatim verbatimChar str =
    let
        predicate =
            \c -> c /= verbatimChar
    in
    case Parser.Advanced.run (ParserTools.text predicate predicate) str of
        Ok stringData ->
            stringData

        Err _ ->
            { content = "", finish = 0, start = 0 }


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


type StackItem
    = Expect StackItemData
    | TextItem { content : String }
    | EndMark String


type ProtoStackItem
    = Expect_ Expectation
    | EndMark_ String


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


type alias StackItemData =
    { expect : Expectation, content : String, count : Int, scanPoint : Int }


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


content : StackItem -> Maybe String
content si =
    case si of
        EndMark _ ->
            Nothing

        TextItem data ->
            Just data.content

        Expect data ->
            Just data.content


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
simpleStackItem stackItem =
    case stackItem of
        Expect si ->
            "Offset " ++ String.fromInt si.scanPoint ++ ": " ++ si.content

        TextItem i ->
            i.content

        EndMark str ->
            str


add : (String -> Element) -> String -> TextCursor -> TextCursor
add parse_ str tc =
    { tc
        | count = tc.count + 1

        -- , stack = TextItem { content = str } :: tc.stack
        , scanPoint = tc.scanPoint + String.length str
        , complete = parse_ str :: tc.parsed ++ tc.complete
        , parsed = []
    }


{-| A
-}
push : String -> ProtoStackItem -> TextCursor -> TextCursor
push prefix proto tc =
    let
        newText =
            -- TODO: thnks about scanPoint
            advance tc (String.dropLeft (tc.scanPoint + 1) tc.source)
                |> Debug.log (Console.magenta "NEW TEXT")

        newContent =
            newText.content

        scanPointIncrement =
            String.length prefix + newText.finish - newText.start

        --  |> Debug.log "scanPointIncrement"
        newStack =
            case proto of
                Expect_ expectation ->
                    Expect
                        { expect = expectation
                        , content = newContent
                        , count = tc.count
                        , scanPoint = tc.scanPoint + scanPointIncrement
                        }
                        :: tc.stack

                EndMark_ prefix_ ->
                    if newContent == "" then
                        EndMark prefix_ :: tc.stack

                    else
                        TextItem { content = newContent } :: EndMark prefix_ :: tc.stack
    in
    { tc
        | count = tc.count + 1
        , scanPoint = tc.scanPoint + scanPointIncrement
        , stack = newStack |> Debug.log (Console.magenta "PUSH, STACK")
    }


pop : (String -> Element) -> String -> TextCursor -> TextCursor
pop parse prefix cursor =
    -- The cursors' scanPoint is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    let
        _ =
            Debug.log (Console.cyan "POP, prefix") prefix
    in
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1, scannerType = NormalScan }

        Just stackTop_ ->
            case stackTop_ of
                Expect _ ->
                    handlePop parse prefix cursor

                --let
                --    data =
                --        showStack (List.reverse cursor.stack)
                --            ++ prefix
                --
                --    _ =
                --        data |> Debug.log (Console.cyan "POP, STACK")
                --
                --    newParsed =
                --        parse data |> AST.simplify |> Debug.log (Console.magenta "newParsed")
                --
                --    _ =
                --        Debug.log (Console.magenta "(SCP, REM)") ( cursor.scanPoint, String.dropLeft cursor.scanPoint cursor.source )
                --
                --    adv =
                --        Debug.log (Console.magenta "ADVANCE") <| advance { cursor | scanPoint = cursor.scanPoint + 1 } (String.dropLeft (cursor.scanPoint + 1) cursor.source)
                --in
                --{ cursor
                --    | stack = []
                --    , parsed = parse data :: cursor.parsed
                --    , count = cursor.count + 1
                --    , scanPoint = cursor.scanPoint + 1 -- + (adv.finish - adv.start)
                --}
                TextItem _ ->
                    -- TODO: fix
                    let
                        _ =
                            Debug.log (Console.cyan "POP TextItem, prefix") prefix
                    in
                    { cursor | count = cursor.count + 1 }

                EndMark _ ->
                    let
                        _ =
                            Debug.log (Console.cyan "POP EndMark, prefix") prefix
                    in
                    handlePop parse prefix cursor


handlePop : (String -> Element) -> String -> TextCursor -> TextCursor
handlePop parse prefix cursor =
    let
        data =
            showStack (List.reverse cursor.stack)
                ++ prefix

        _ =
            data |> Debug.log (Console.cyan "POP, STACK")

        newParsed =
            parse data |> AST.simplify |> Debug.log (Console.magenta "newParsed")

        _ =
            Debug.log (Console.magenta "(SCP, REM)") ( cursor.scanPoint, String.dropLeft cursor.scanPoint cursor.source )

        adv =
            Debug.log (Console.magenta "ADVANCE") <| advance { cursor | scanPoint = cursor.scanPoint + 1 } (String.dropLeft (cursor.scanPoint + 1) cursor.source)
    in
    { cursor
        | stack = []
        , parsed = parse data :: cursor.parsed
        , count = cursor.count + 1
        , scanPoint = cursor.scanPoint + 1 -- + (adv.finish - adv.start)
    }


handleText : (String -> Element) -> String -> StackItemData -> TextCursor -> TextCursor
handleText parse prefix stackTopData cursor =
    let
        _ =
            Debug.log (Console.magenta "IN handleText, STACK") cursor.stack
    in
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1 }

        Just stackTop_ ->
            let
                ( fname, args_ ) =
                    content stackTop_
                        |> Maybe.map String.words
                        |> Maybe.andThen List.Extra.uncons
                        |> Maybe.withDefault ( "fname", [] )

                args =
                    List.map (\a -> Text a MetaData.dummy) args_

                parsed : List Element
                parsed =
                    case stackTopData.expect.etype of
                        ElementType ->
                            let
                                reallyNew =
                                    ""

                                new =
                                    handleFunction parse cursor stackTop_ fname args
                            in
                            case ( List.head new, cursor.text ) of
                                ( Nothing, _ ) ->
                                    new

                                ( _, "" ) ->
                                    new

                                ( Just first_, text ) ->
                                    [ AST.join first_ (List.drop 1 new ++ [ parse text ]) ]

                        CodeType ->
                            [ Element (Name "code") (Text stackTopData.content MetaData.dummy) MetaData.dummy ] ++ cursor.parsed

                        InlineMathType ->
                            [ Element (Name "math2") (Text stackTopData.content MetaData.dummy) MetaData.dummy ] ++ cursor.parsed

                        QuotedType ->
                            [ Text (Library.Utility.unquote stackTopData.content) MetaData.dummy ] ++ cursor.parsed
            in
            { cursor
                | parsed = parsed

                --, complete = complete
                , stack = List.drop 1 cursor.stack
                , scanPoint = cursor.scanPoint + 1
                , count = cursor.count + 1
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

    else
        [ AST.join (Element (AST.Name fname) (EList args MetaData.dummy) MetaData.dummy) tc.parsed ]


commit : (String -> Element) -> TextCursor -> TextCursor
commit parse cursor =
    cursor |> commit_ parse |> (\tc2 -> { tc2 | complete = List.reverse tc2.complete })


commit_ : (String -> Element) -> TextCursor -> TextCursor
commit_ parse tc =
    let
        parsed =
            if tc.text == "" then
                tc.parsed

            else
                AST.Text tc.text MetaData.dummy :: tc.parsed

        newParsed =
            parse (showStack tc.stack)

        complete =
            parsed ++ tc.complete
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                complete_ =
                    case endSymbol top of
                        Nothing ->
                            let
                                parsed_ =
                                    newParsed :: tc.parsed |> Debug.log (Console.magenta "parsed_")
                            in
                            if String.left 1 (beginSymbol top) == "#" then
                                handleHeadings tc top parsed_

                            else if beginSymbol top == ":" then
                                handleLineCommand tc parsed_

                            else
                                --let
                                --    errorMessage =
                                --        StackError (scanPoint top) tc.scanPoint ("((unknown delimiter " ++ beginSymbol top ++ " at position " ++ String.fromInt (scanPoint top) ++ "))") (String.slice (scanPoint top) tc.scanPoint tc.source)
                                --in
                                List.reverse parsed_

                        Just _ ->
                            handleError tc top
            in
            commit parse
                { tc
                    | count = 1 + tc.count
                    , text = ""
                    , stack = restOfStack
                    , parsed = []
                    , complete = complete_
                }


handleHeadings : TextCursor -> StackItem -> List Element -> List Element
handleHeadings tc top_ parsed_ =
    case top_ of
        EndMark _ ->
            []

        TextItem _ ->
            []

        Expect top ->
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
        ++ [ Element (Name "error") (Text (" unmatched " ++ beginSymbol top ++ " ") MetaData.dummy) MetaData.dummy
           , Text (beginSymbol top) MetaData.dummy
           ]
        ++ List.reverse tc.parsed
        ++ [ Text tc.text MetaData.dummy ]



-- PREDICATES


{-| The parser has paused at charater c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Configuration -> TextCursor -> String -> Bool
canPop configuration_ tc prefix =
    isReducibleWith prefix tc.stack |> Debug.log (Console.magenta "canPop")



--if canPopPrecondition configuration_ tc prefix then
--    case List.head tc.stack of
--        Nothing ->
--            False
--
--        Just stackTop ->
--            -- True
--            Maybe.map2 String.contains (endSymbol stackTop) (Just prefix) |> Maybe.withDefault False
--    -- TODO why should the above check be necessary?
--    -- With it, we get __many__ failing tests
--
--else
--    False


canPopPrecondition : Configuration -> TextCursor -> String -> Bool
canPopPrecondition configuration_ tc prefix =
    let
        isEndSymbol =
            Config.isEndSymbol configuration_ tc.scanPoint prefix
    in
    if isEndSymbol then
        True

    else if String.length prefix > 1 then
        canPopPrecondition configuration_ tc (String.dropLeft 1 prefix)

    else
        False


canPush : Configuration -> TextCursor -> String -> { value : Bool, prefix : String }
canPush configuration_ tc prefix =
    if prefix == "" then
        { value = False, prefix = "" }

    else if canPush_ configuration_ tc prefix then
        { value = True, prefix = prefix }

    else
        canPush configuration_ tc (String.dropLeft 1 prefix)



--  |> Debug.log (Console.magenta "canPush, prefix " ++ prefix)
--(Config.isBeginSymbol configuration_ tc.scanPoint prefix
--    || (Config.isEndSymbol configuration_ tc.scanPoint prefix
--            && not
--                (isReducibleWith prefix tc.stack |> Debug.log (Console.magenta "isReducibleWith"))
--       )
--)
--    |> Debug.log (Console.magenta "canPush")


canPush_ configuration_ tc prefix =
    (Config.isBeginSymbol configuration_ tc.scanPoint prefix
        || ((Config.isEndSymbol configuration_ tc.scanPoint prefix |> Debug.log (Console.magenta "isEndSymbol, prefix = " ++ prefix))
                && (isNotReducibleWith prefix tc.stack |> Debug.log (Console.magenta "isNotReducible with prefix = " ++ prefix))
           )
    )
        |> Debug.log (Console.magenta "canPush_, prefix = " ++ prefix)



-- PRINT
