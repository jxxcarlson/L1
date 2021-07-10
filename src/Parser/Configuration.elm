module Parser.Configuration exposing (expectations)

import Parser.Config exposing (EType(..), MarkPosition(..))


expectations =
    [ { beginChar = '[', expectedEndChar = Just ']', etype = ElementType, isVerbatim = False, markPosition = Anywhere }
    , { beginChar = '`', expectedEndChar = Just '`', etype = CodeType, isVerbatim = True, markPosition = Anywhere }
    , { beginChar = '$', expectedEndChar = Just '$', etype = InlineMathType, isVerbatim = True, markPosition = Anywhere }
    , { beginChar = '#', expectedEndChar = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginChar = ':', expectedEndChar = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { beginChar = '"', expectedEndChar = Just '"', etype = QuotedType, isVerbatim = False, markPosition = Anywhere }
    ]
