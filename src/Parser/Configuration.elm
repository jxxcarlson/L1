module Parser.Configuration exposing (expectations)

import Parser.Config exposing (EType(..), MarkPosition(..))


expectations =
    [ { begin = '[', end = Just ']', etype = ElementType, isVerbatim = False, markPosition = Anywhere }
    , { begin = '`', end = Just '`', etype = CodeType, isVerbatim = True, markPosition = Anywhere }
    , { begin = '$', end = Just '$', etype = InlineMathType, isVerbatim = True, markPosition = Anywhere }
    , { begin = '#', end = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { begin = ':', end = Nothing, etype = ElementType, isVerbatim = False, markPosition = AtBeginning }
    , { begin = '"', end = Just '"', etype = QuotedType, isVerbatim = False, markPosition = Anywhere }
    ]
