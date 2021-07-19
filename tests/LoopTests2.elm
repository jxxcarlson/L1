module LoopTests2 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser.AST exposing (Element_(..), Name(..), VerbatimType(..), simplify)
import Parser.Driver exposing (pl)
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "The Driver.parseLoop function"
            [ test "a" <|
                \_ ->
                    "a"
                        |> pl
                        |> Expect.equal [ Text_ "a" ]
            , test "[a] b" <|
                \_ ->
                    "[a] b"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ []), Text_ " b" ]
            , test "[a b]" <|
                \_ ->
                    "[a b]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]) ]
            , test "a [b c]" <|
                \_ ->
                    "a [b c]"
                        |> pl
                        |> Expect.equal [ Text_ "a ", Element_ (Name "b") (EList_ [ Text_ "c" ]) ]
            , test "[a b] c" <|
                \_ ->
                    "[a b] c"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " c" ]
            , test "[a b] [c d]" <|
                \_ ->
                    "[a b] [c d]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "[a b] x [c d]" <|
                \_ ->
                    "[a b] x [c d]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " x ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "u [a b] x [c d]" <|
                \_ ->
                    "u [a b] x [c d]"
                        |> pl
                        |> Expect.equal [ Text_ "u ", Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " x ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "[x [a b]]" <|
                \_ ->
                    "[x [a b]]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "x") (EList_ [ Element_ (Name "a") (EList_ [ Text_ "b " ]) ]) ]
            , test "a b [c d] e [f g]" <|
                \_ ->
                    "a b [c d] e [f g]"
                        |> pl
                        |> Expect.equal [ Text_ "a b ", Element_ (Name "c") (EList_ [ Text_ "d" ]), Text_ " e ", Element_ (Name "f") (EList_ [ Text_ "g" ]) ]
            , test "[a b [c d] e [f g]]" <|
                \_ ->
                    "[a b [c d] e [f g]]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b  ", Element_ (Name "c") (EList_ [ Text_ "d " ]), Text_ "  e  ", Element_ (Name "f") (EList_ [ Text_ "g " ]) ]) ]
            , test "abc def [foo] ghi jkl [bar] mno pqr" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl
                        |> Expect.equal [ Text_ "abc def ", Element_ (Name "foo") (EList_ []), Text_ " ghi jkl ", Element_ (Name "bar") (EList_ []), Text_ " mno pqr" ]
            , test "$a^2$" <|
                \_ ->
                    "$a^2$"
                        |> pl
                        |> Expect.equal [ Verbatim_ Math "a^2" ]
            , test "`a^2`" <|
                \_ ->
                    "`a^2`"
                        |> pl
                        |> Expect.equal [ Verbatim_ Code "a^2" ]
            , test "\"a^2\"" <|
                \_ ->
                    "\"a^2\""
                        |> pl
                        |> Expect.equal [ Verbatim_ Quoted "a^2" ]
            , test "# Images" <|
                \_ ->
                    "# Images"
                        |> pl
                        |> Expect.equal [ Element_ (Name "heading") (EList_ [ Text_ "# Images" ]) ]
            , test "## Images" <|
                \_ ->
                    "## Images"
                        |> pl
                        |> Expect.equal [ Element_ (Name "heading") (EList_ [ Text_ "## Images" ]) ]
            , test "[i w:8 p:l https://foo.bar]" <|
                \_ ->
                    "[i w:8 p:l https://foo.bar]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "i") (EList_ [ Text_ "w:8 p:l https://foo.bar" ]) ]
            ]
