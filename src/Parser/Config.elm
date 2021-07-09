module Parser.Config exposing (standard)


type alias Config =
    { startMarks : List Char
    , endMarks : List Char
    , marks : List Char
    }


standard : Config
standard =
    { startMarks = [ '[' ]
    , endMarks = [ ']' ]
    , marks = ['[', ']']
    }
