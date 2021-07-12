module TextCursorTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser.AST exposing (Element_(..), Name(..), simplify)
import Parser.Driver exposing (pl)
import Parser.TextCursor exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Driver.parseLoop function"
        [ test "pure text input" <|
            \_ ->
                "foo"
                    |> pl
                    |> Expect.equal
                        [ Text_ "foo"
                        ]
        ]


cursor1 =
    init 0 "abc [foo]"
