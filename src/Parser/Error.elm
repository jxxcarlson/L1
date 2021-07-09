module Parser.Error exposing (Context(..), Problem(..), heading)


type Problem
    = ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingPipe
    | EndOfInput
    | ExpectingEscape
    | ExpectingComma
    | ExpectingRawPrefix
    | ExpectingRawStringBegin
    | ExpectingRawStringEnd
    | UnHandledError Int
    | NoError


heading : Problem -> String
heading problem =
    case problem of
        ExpectingRightBracket ->
            "Missing right bracket?"

        ExpectingLeftBracket ->
            "Missing left bracket?"

        ExpectingPipe ->
            "Missing pipe symbol?"

        _ ->
            "Error in"


type Context
    = CElement
    | CArgs
    | CBody
    | CArgsAndBody
    | TextExpression
