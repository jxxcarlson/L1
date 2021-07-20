module Parser.Configuration exposing (configuration)

import Parser.Config as Config exposing (EType(..), Expectation, MarkPosition(..))


configuration =
    Config.configure expectations


{-| The actual configuration used by the parser is derived from a List Expectation
by applying the function Config.configure : List Expectation -> Configuration.

Scenario: the scanPoint field of the text cursor points to the character '['.
Because this character is in the list Configuration.beginChars, the parser
knows that it should push the expectation

    { beginSymbol = "[\n"
    , endSymbol = Just "]"
    , etype = ElementType
    , isVerbatim = False
    , markPosition = Anywhere
    }

onto the stack. If later cursor.scanPoint targets the character ']', it will
know that it should pop this element off the stack. To recall: the permissible
opreration on the cursor are

    ADD, PUSH, POP, and COMMIT

-}
expectations : List Expectation
expectations =
    [ { beginSymbol = "[", endSymbol = Just "]", etype = ElementType, isVerbatim = False, markPosition = Anywhere }
    , { beginSymbol = "`", endSymbol = Just "`", etype = CodeType, isVerbatim = True, markPosition = Anywhere }
    , { beginSymbol = "$", endSymbol = Just "$", etype = InlineMathType, isVerbatim = True, markPosition = Anywhere }
    , { beginSymbol = "#", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "##", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "###", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "####", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = ":", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "\"", endSymbol = Just "\"", etype = QuotedType, isVerbatim = True, markPosition = Anywhere }
    , { beginSymbol = "||", endSymbol = Nothing, etype = CodeType, isVerbatim = True, markPosition = AtBeginning }
    , { beginSymbol = "|", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    ]
