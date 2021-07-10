module Parser.Configuration exposing (expectations)

import Parser.Config exposing (EType(..))


expectations =
    [ { begin = '[', end = Just ']', etype = ElementType, isVerbatim = False }
    , { begin = '`', end = Just '`', etype = CodeType, isVerbatim = True }
    , { begin = '$', end = Just '$', etype = InlineMathType, isVerbatim = True }
    , { begin = '#', end = Nothing, etype = ElementType, isVerbatim = False }
    , { begin = '"', end = Just '"', etype = QuotedType, isVerbatim = False }
    ]
