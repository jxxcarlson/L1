module L1.Parser.Parser exposing
    ( elementName
    , hashMarks
    , identifier
    , parse
    , parseHeading
    , parseItem
    , parseList
    , parseSimple
    , parser
    )

import L1.Library.ParserTools as T
import L1.Library.StringParser as XString
import L1.Parser.AST as AST exposing (Element(..), Name(..), VerbatimType(..))
import L1.Parser.Error exposing (Context(..), Problem(..))
import L1.Parser.Loc as Loc exposing (StringPosition)
import L1.Parser.MetaData as MetaData exposing (MetaData)
import List
import Parser.Advanced as Parser exposing ((|.), (|=))


type alias ErrorMessage =
    String


type alias Parser a =
    Parser.Parser Context Problem a


type alias ParseError =
    Parser.DeadEnd Context Problem



-- PARSER


parse : Int -> Loc.ChunkLocation -> Int -> String -> Element
parse generation chunkLocation scanPosition str =
    case Parser.run (parser generation chunkLocation scanPosition) str of
        Ok ast ->
            ast

        Err errors ->
            Problem errors str


parseSimple =
    parse 0 { chunkIndex = 0, firstLine = 0 } 0 >> AST.simplify


parseItem : Int -> Loc.ChunkLocation -> Int -> String -> Result (List ParseError) Element
parseItem generation chunkLocation scanPosition str =
    Parser.run (itemParser generation chunkLocation scanPosition) str


parseHeading : Int -> Loc.ChunkLocation -> Int -> String -> Result (List ParseError) Element
parseHeading generation chunkLocation scanPosition str =
    Parser.run (headingParser generation chunkLocation scanPosition) str


parseList : Int -> Loc.ChunkLocation -> Int -> String -> Result (List ParseError) (List Element)
parseList generation chunkLocation lineNumber str =
    Parser.run (listParser generation chunkLocation lineNumber) str


listParser : Int -> Loc.ChunkLocation -> Int -> Parser (List Element)
listParser generation chunkLocation scanPosition =
    T.many (parser generation chunkLocation scanPosition)


parser : Int -> Loc.ChunkLocation -> Int -> Parser Element
parser generation chunkLocation scanPosition =
    Parser.oneOf
        [ primitiveElement generation chunkLocation scanPosition
        , mathElement generation chunkLocation scanPosition
        , quotedElement generation chunkLocation scanPosition
        , codeElement generation chunkLocation scanPosition
        , plainText generation chunkLocation scanPosition
        ]


{-|

> run (primitiveElement 0 0) "[strong |0| stuff]"
> Ok (Element "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (primitiveElement 0 0) "[strong stuff]"
> Ok (Element "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
primitiveElement : Int -> Loc.ChunkLocation -> Int -> Parser Element
primitiveElement generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start name body_ end source -> Element name body_ (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. leftBracket
            |= Parser.oneOf [ elementName |> Parser.map Name, Parser.succeed UndefinedName ]
            |= argsAndBody generation chunkLocation scanPosition
            |. Parser.spaces
            |. rightBracket
            |= Parser.getOffset
            |= Parser.getSource


mathElement : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
mathElement generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Math content (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. dollarSign
            |= string [ '$' ]
            |. dollarSign
            |= Parser.getOffset
            |= Parser.getSource


codeElement : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
codeElement generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Code content (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. backTick
            |= string [ '`' ]
            |. backTick
            |= Parser.getOffset
            |= Parser.getSource


quotedElement : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
quotedElement generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start content end source -> Verbatim Quoted content (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. quoteMark
            |= string [ '"' ]
            |. quoteMark
            |= Parser.getOffset
            |= Parser.getSource


headingParser : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
headingParser generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start n elements end source -> Element (Name ("heading" ++ String.fromInt n)) elements (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |= hashMarks
            |. Parser.chompIf (\c -> c == ' ') ExpectingSpace
            |. Parser.chompWhile (\c -> c == ' ')
            |= listParser generation chunkLocation 0
            |= Parser.getOffset
            |= Parser.getSource


itemParser : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
itemParser generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start elements end source -> Element (Name "item") elements (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. colonMark
            |. Parser.chompWhile (\c -> c == ' ')
            |= listParser generation chunkLocation 0
            |= Parser.getOffset
            |= Parser.getSource


parseBlock : Int -> Loc.ChunkLocation -> Int -> String -> Result (List ParseError) Element
parseBlock generation chunkLocation scanPosition str =
    Parser.run (blockParser generation chunkLocation scanPosition) str


blockParser : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem Element
blockParser generation chunkLocation scanPosition =
    Parser.inContext CElement <|
        -- TODO: is this correct?
        Parser.succeed (\start name elements end source -> Element (Name name.content) elements (meta generation chunkLocation scanPosition start end))
            |= Parser.getOffset
            |. pipeMark
            --|. Parser.spaces
            |= T.text Char.isAlpha Char.isAlpha
            |. Parser.chompWhile (\c -> c == ' ')
            |= listParser generation chunkLocation 0
            |= Parser.getOffset
            |= Parser.getSource


pipeMark =
    Parser.symbol (Parser.Token "|" ExpectingPipe)


hashMarks : Parser Int
hashMarks =
    Parser.succeed (\start end -> end - start)
        |= Parser.getOffset
        |. hashMark
        |. Parser.chompWhile (\c -> c == '#')
        |= Parser.getOffset


elementName =
    T.first identifier Parser.spaces


identifier =
    T.text Char.isAlpha (\c -> not <| List.member c [ '[', ']', ' ', '\n' ])
        |> Parser.map .content


argsAndBody : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem (List Element)
argsAndBody generation chunkLocation scanPosition =
    Parser.inContext CArgsAndBody <| elementBody generation chunkLocation scanPosition


metaOfList generation list =
    { generation = generation, position = list |> List.map (\el -> AST.position el) |> Loc.positionOfList }


elementBody : Int -> Loc.ChunkLocation -> Int -> Parser.Parser Context Problem (List Element)
elementBody generation chunkLocation scanPosition =
    Parser.inContext CBody <|
        -- Parser.lazy (\_ -> T.many (parser generation) |> Parser.map (\list -> EList list (metaOfList generation list)))
        Parser.lazy (\_ -> T.many (parser generation chunkLocation scanPosition))



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


meta : Int -> Loc.ChunkLocation -> Int -> Int -> Int -> MetaData
meta generation chunkLocation scanPoint start finish =
    let
        stringLocation =
            { start = start + Debug.log "SCAN POINT" scanPoint, end = finish - 1 + scanPoint }
    in
    { position = stringLocation
    , generation = generation
    , location = chunkLocation
    , id = MetaData.makeId generation chunkLocation stringLocation
    , info = Nothing
    }


plainText : Int -> Loc.ChunkLocation -> Int -> Parser Element
plainText generation chunkLocation scanPosition =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Text data.content (meta generation chunkLocation scanPosition data.start data.finish))
        )


textWithPredicate : (Char -> Bool) -> Int -> Loc.ChunkLocation -> Int -> Parser Element
textWithPredicate predicate generation chunkLocation scanPosition =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate predicate
            |> Parser.map (\data -> Text data.content (meta generation chunkLocation scanPosition data.start data.finish))
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
