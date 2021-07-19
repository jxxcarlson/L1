module Parser.TextCursor exposing
    ( TextCursor, init
    , ProtoStackItem(..), ScannerType(..), add, advance, canPop, canPush, commit, configuration, pop, push
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
import Parser.Stack as Stack exposing (StackItem)


configuration =
    Config.configure Configuration.expectations


advance : TextCursor -> String -> ParserTools.StringData
advance cursor textToProcess =
    case cursor.scannerType of
        NormalScan ->
            advanceNormal configuration
                cursor.scanPoint
                textToProcess

        VerbatimScan c ->
            advanceVerbatim c textToProcess


{-| Return the longest prefix of str that does not contain a delimiter.
The delimiter sets used depend upon position. One set for position = 0,
another for position /= 0.
-}
advanceNormal : Configuration -> Int -> String -> ParserTools.StringData
advanceNormal config position str =
    let
        _ =
            Debug.log (Console.cyan "advanceNormal, str") str

        delimiterTypes =
            if String.slice 0 1 str == ":" then
                Config.AllDelimiters

            else
                Config.InteriorDelimiters
    in
    case Parser.Advanced.run (ParserTools.text (Config.notDelimiter configuration delimiterTypes) (Config.notDelimiter configuration delimiterTypes)) str of
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


type ProtoStackItem
    = Expect_ Expectation
    | EndMark_ String


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
            -- TODO: think about scanPoint
            advance tc (String.dropLeft (tc.scanPoint + String.length prefix) tc.source)

        --|> Debug.log (Console.magenta "NEW TEXT")
        newContent =
            newText.content

        scanPointIncrement =
            String.length prefix + newText.finish - newText.start |> Debug.log (Console.yellow "PUSH, scanpoint increment")

        --  |> Debug.log "scanPointIncrement"
        newStack =
            case proto of
                Expect_ expectation ->
                    Stack.Expect
                        { expect = expectation
                        , content = newContent
                        , count = tc.count
                        , scanPoint = tc.scanPoint + scanPointIncrement
                        }
                        :: tc.stack

                EndMark_ prefix_ ->
                    if newContent == "" then
                        Stack.EndMark prefix_ :: tc.stack

                    else
                        Stack.TextItem { content = newContent } :: Stack.EndMark prefix_ :: tc.stack
    in
    { tc
        | count = tc.count + 1
        , scanPoint = tc.scanPoint + scanPointIncrement
        , stack = newStack -- |> Debug.log (Console.magenta "PUSH, STACK")
    }


pop : (String -> Element) -> String -> TextCursor -> TextCursor
pop parse prefix cursor =
    -- The cursors' scanPoint is pointing at a character that
    -- signal the end of an element, e.g., ']' in the
    -- case of language L1.  It is time to pop the stack
    -- and update the cursor.  We split this operation into
    -- two case, depending on whether cursor.text is empty.
    --let
    --    _ =
    --        Debug.log (Console.cyan "POP, prefix") prefix
    --in
    let
        _ =
            Debug.log (Console.cyan "POP, prefix") prefix
    in
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1, scannerType = NormalScan }

        Just stackTop_ ->
            case stackTop_ of
                Stack.Expect _ ->
                    handlePop parse prefix stackTop_ cursor

                Stack.TextItem _ ->
                    { cursor | count = cursor.count + 1 }

                Stack.EndMark _ ->
                    handlePop parse prefix stackTop_ cursor


handlePop : (String -> Element) -> String -> StackItem -> TextCursor -> TextCursor
handlePop parse prefix stackTop cursor =
    let
        data =
            Stack.showStack (List.reverse cursor.stack)
                ++ prefix

        parsed =
            case Stack.etype stackTop of
                Just ElementType ->
                    parse data

                Just CodeType ->
                    Element (Name "code") (Text (Stack.content stackTop |> Maybe.withDefault "") MetaData.dummy) MetaData.dummy

                Just InlineMathType ->
                    Element (Name "math2") (Text (Stack.content stackTop |> Maybe.withDefault "") MetaData.dummy) MetaData.dummy

                Just QuotedType ->
                    Text (Library.Utility.unquote (Stack.content stackTop |> Maybe.withDefault "")) MetaData.dummy

                Nothing ->
                    parse data
    in
    { cursor
        | stack = []
        , parsed = parsed :: cursor.parsed
        , count = cursor.count + 1
        , scanPoint = cursor.scanPoint + 1 -- + (adv.finish - adv.start)
    }


handleText : (String -> Element) -> String -> Stack.StackItemData -> TextCursor -> TextCursor
handleText parse prefix stackTopData cursor =
    --let
    --    _ =
    --        Debug.log (Console.magenta "IN handleText, STACK") cursor.stack
    --in
    case List.head cursor.stack of
        Nothing ->
            { cursor | count = cursor.count + 1, scanPoint = cursor.scanPoint + 1 }

        Just stackTop_ ->
            let
                ( fname, args_ ) =
                    Stack.content stackTop_
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



---- XXXXX ----


commit2_ : (String -> Element) -> TextCursor -> TextCursor
commit2_ parse tc =
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = tc.parsed }

        top :: restOfStack ->
            if Stack.isReducible tc.stack then
                { tc | complete = tc.complete ++ List.map (Stack.show >> parse) tc.stack }

            else
                tc



---- XXXXX ----


commit_ : (String -> Element) -> TextCursor -> TextCursor
commit_ parse tc =
    let
        parsed =
            if tc.text == "" then
                tc.parsed

            else
                AST.Text tc.text MetaData.dummy :: tc.parsed

        newParsed =
            parse (Stack.showStack tc.stack)

        complete =
            parsed ++ tc.complete
    in
    case tc.stack of
        [] ->
            { tc | parsed = [], complete = complete }

        top :: restOfStack ->
            let
                complete_ =
                    case Stack.endSymbol top of
                        Nothing ->
                            let
                                parsed_ =
                                    newParsed :: tc.parsed

                                --|> Debug.log (Console.magenta "parsed_")
                            in
                            if String.left 1 (Stack.beginSymbol top) == "#" then
                                handleHeadings tc top parsed_

                            else if Stack.beginSymbol top == ":" then
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
        Stack.EndMark _ ->
            []

        Stack.TextItem _ ->
            []

        Stack.Expect top ->
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
        ++ [ Element (Name "error") (Text (" unmatched " ++ Stack.beginSymbol top ++ " ") MetaData.dummy) MetaData.dummy
           , Text (Stack.beginSymbol top) MetaData.dummy
           ]
        ++ List.reverse tc.parsed
        ++ [ Text tc.text MetaData.dummy ]



-- PREDICATES


{-| The parser has paused at charater c. If the prefix of the
remaining source text that begins with character c what we expect?
-}
canPop : Configuration -> TextCursor -> String -> Bool
canPop configuration_ tc prefix =
    let
        _ =
            Debug.log (Console.cyan "canPop, isReducibleWith") ( prefix, tc.stack |> Stack.simplifyStack )
    in
    Stack.isReducibleWith prefix tc.stack |> Debug.log (Console.cyan "canPop")



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
    if Config.isVerbatimSymbol prefix then
        if Just prefix == (List.head tc.stack |> Maybe.map Stack.beginSymbol) then
            { value = False, prefix = prefix } |> Debug.log (Console.yellow "B 1")

        else
            { value = True, prefix = prefix } |> Debug.log (Console.yellow "B 2")

    else
        canPush1 configuration_ tc prefix |> Debug.log (Console.yellow "B 3")


canPush1 : Configuration -> TextCursor -> String -> { value : Bool, prefix : String }
canPush1 configuration_ tc prefix =
    (if prefix == "" then
        { value = False, prefix = "" }

     else if canPush2 configuration_ tc prefix then
        { value = True, prefix = prefix }

     else
        canPush configuration_ tc (String.dropLeft 1 prefix)
    )
        |> Debug.log (Console.cyan "canPush")


canPush2 configuration_ tc prefix =
    Config.isBeginSymbol configuration_ tc.scanPoint prefix
        || (Config.isEndSymbol configuration_ tc.scanPoint prefix
                && Stack.isNotReducibleWith prefix tc.stack
           )



-- PRINT
