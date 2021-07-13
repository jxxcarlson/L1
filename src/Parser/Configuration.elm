module Parser.Configuration exposing (expectations)

import Parser.Config exposing (EType(..), MarkPosition(..))


expectations =
    [ { beginSymbol = "[", endSymbol = Just "]", etype = ElementType, isVerbatim = False, markPosition = Anywhere }
    , { beginSymbol = "`", endSymbol = Just "`", etype = CodeType, isVerbatim = True, markPosition = Anywhere }
    , { beginSymbol = "$", endSymbol = Just "$", etype = InlineMathType, isVerbatim = True, markPosition = Anywhere }
    , { beginSymbol = "#", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "##", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "###", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "####", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = ":", endSymbol = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginSymbol = "\"", endSymbol = Just "\"", etype = QuotedType, isVerbatim = False, markPosition = Anywhere }
    , { beginSymbol = "```", endSymbol = Nothing, etype = CodeType, isVerbatim = True, markPosition = AtBeginning }
    ]
