module Parser.Parser exposing (hashMarks, parse, parseHeading, parseItem, parseList, parseSimple, parser)

import Library.ParserTools as T
import Library.StringParser as XString
import Parser.AST as AST exposing (Element(..), Name(..), VerbatimType(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Loc as Loc exposing (Position)


type alias ErrorMessage =
    String


type alias Parser a =
    Parser.Parser Context Problem a


type alias ParseError =
    Parser.DeadEnd Context Problem



-- PARSER


parse : Int -> String -> Element
parse generation str =
    case Parser.run (parser generation) str of
        Ok ast ->
            ast

        Err errors ->
            Problem errors str


parseSimple =
    parse 0 >> AST.simplify


parseItem : Int -> String -> Result (List ParseError) Element
parseItem generation str =
    Parser.run (itemParser generation) str


parseHeading : Int -> String -> Result (List ParseError) Element
parseHeading generation str =
    Parser.run (headingParser generation) str


parseList : Int -> Int -> String -> Result (List ParseError) (List Element)
parseList generation lineNumber str =
    Parser.run (listParser generation lineNumber) str


listParser : Int -> Int -> Parser (List Element)
listParser generation lineNumber =
    T.many (parser generation)


parser : Int -> Parser Element
parser generation =
    Parser.oneOf [ primitiveElement generation, mathElement generation, quotedElement generation, codeElement generation, plainText generation ]


{-|

> run (primitiveElement 0 0) "[strong |0| stuff]"
> Ok (Element "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (primitiveElement 0 0) "[strong stuff]"
> Ok (Element "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
primitiveElement : Int -> Parser Element
primitiveElement generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start name body_ end source -> Element name body_ (meta generation start end))
            |= Parser.getOffset
            |. leftBracket
            |= Parser.oneOf [ elementName |> Parser.map Name, Parser.succeed Undefined ]
            |= argsAndBody generation
            |. Parser.spaces
            |. rightBracket
            |= Parser.getOffset
            |= Parser.getSource


mathElement generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Math content (meta generation start end))
            |= Parser.getOffset
            |. dollarSign
            |= string [ '$' ]
            |. dollarSign
            |= Parser.getOffset
            |= Parser.getSource


codeElement generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Code content (meta generation start end))
            |= Parser.getOffset
            |. backTick
            |= string [ '`' ]
            |. backTick
            |= Parser.getOffset
            |= Parser.getSource


quotedElement generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Quoted content (meta generation start end))
            |= Parser.getOffset
            |. quoteMark
            |= string [ '"' ]
            |. quoteMark
            |= Parser.getOffset
            |= Parser.getSource


headingParser generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start n elements end source -> Element (Name ("heading" ++ String.fromInt n)) (EList elements (meta generation start end)) (meta generation start end))
            |= Parser.getOffset
            |= hashMarks
            |. Parser.chompIf (\c -> c == ' ') ExpectingSpace
            |. Parser.chompWhile (\c -> c == ' ')
            |= listParser generation 0
            |= Parser.getOffset
            |= Parser.getSource


itemParser generation =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start elements end source -> Element (Name "item") (EList elements (meta generation start end)) (meta generation start end))
            |= Parser.getOffset
            |. colonMark
            |. Parser.chompWhile (\c -> c == ' ')
            |= listParser generation 0
            |= Parser.getOffset
            |= Parser.getSource


hashMarks : Parser Int
hashMarks =
    Parser.succeed (\start end -> end - start)
        |= Parser.getOffset
        |. hashMark
        |. Parser.chompWhile (\c -> c == '#')
        |= Parser.getOffset


elementName =
    T.first (string_ [ '[', ']', ' ', '\n' ]) Parser.spaces


argsAndBody : Int -> Parser.Parser Context Problem Element
argsAndBody generation =
    Parser.inContext CArgsAndBody <| elementBody generation


metaOfList generation list =
    { generation = generation, position = list |> List.map (\el -> AST.position el) |> Loc.positionOfList }


elementBody generation =
    Parser.inContext CBody <|
        Parser.lazy (\_ -> T.many (parser generation) |> Parser.map (\list -> EList list (metaOfList generation list)))



-- TOOLS


isProblem : Element -> Bool
isProblem element =
    case element of
        Problem _ _ ->
            True

        _ ->
            False


hasProblem : List Element -> Bool
hasProblem elements =
    List.foldl (\e acc -> isProblem e || acc) False elements



-- TEXT AND STRINGS


meta generation start finish =
    { position = { start = start, end = finish }, generation = generation }


plainText : Int -> Parser Element
plainText generation =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Text data.content (meta generation data.start data.finish))
        )


textWithPredicate : (Char -> Bool) -> Int -> Parser Element
textWithPredicate predicate generation =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate predicate
            |> Parser.map (\data -> Text data.content (meta generation data.start data.finish))
        )


string stopChars =
    T.first (string_ stopChars) Parser.spaces


string_ : List Char -> Parser String
string_ stopChars =
    rawText_ stopChars |> Parser.map .content


rawText_ : List Char -> Parser { start : Int, length : Int, content : String }
rawText_ stopChars =
    Parser.succeed (\begin end content -> { start = begin, length = end - begin, content = String.slice begin end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


dollarSign =
    Parser.symbol (Parser.Token "$" ExpectingDollarSign)


colonMark =
    Parser.symbol (Parser.Token ":" ExpectingColon)


hashMark =
    Parser.symbol (Parser.Token "#" ExpectingHashMark)


quoteMark =
    Parser.symbol (Parser.Token "\"" ExpectingQuoteMark)


backTick =
    Parser.symbol (Parser.Token "`" ExpectingBackTick)


leftBracket =
    Parser.symbol (Parser.Token "[" ExpectingLeftBracket)


rightBracket =
    Parser.symbol (Parser.Token "]" ExpectingRightBracket)
