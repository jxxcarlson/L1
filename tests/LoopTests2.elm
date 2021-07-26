module LoopTests2 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Parser.AST exposing (Element_(..), Name(..), VerbatimType(..), simplify)
import L1.Parser.Chunk exposing (pl_)
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "The Driver.parseLoop function"
            [ test "a" <|
                \_ ->
                    "a"
                        |> pl_
                        |> Expect.equal [ Text_ "a" ]
            , test "[a] b" <|
                \_ ->
                    "[a] b"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ []), Text_ " b" ]
            , test "[a b]" <|
                \_ ->
                    "[a b]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]) ]
            , test "a [b c]" <|
                \_ ->
                    "a [b c]"
                        |> pl_
                        |> Expect.equal [ Text_ "a ", Element_ (Name "b") (EList_ [ Text_ "c" ]) ]
            , test "[a b] c" <|
                \_ ->
                    "[a b] c"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " c" ]
            , test "[a b] [c d]" <|
                \_ ->
                    "[a b] [c d]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "[a b] x [c d]" <|
                \_ ->
                    "[a b] x [c d]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " x ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "u [a b] x [c d]" <|
                \_ ->
                    "u [a b] x [c d]"
                        |> pl_
                        |> Expect.equal [ Text_ "u ", Element_ (Name "a") (EList_ [ Text_ "b" ]), Text_ " x ", Element_ (Name "c") (EList_ [ Text_ "d" ]) ]
            , test "[x [a b]]" <|
                \_ ->
                    "[x [a b]]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "x") (EList_ [ Element_ (Name "a") (EList_ [ Text_ "b " ]) ]) ]
            , test "a b [c d] e [f g]" <|
                \_ ->
                    "a b [c d] e [f g]"
                        |> pl_
                        |> Expect.equal [ Text_ "a b ", Element_ (Name "c") (EList_ [ Text_ "d" ]), Text_ " e ", Element_ (Name "f") (EList_ [ Text_ "g" ]) ]
            , test "[a b [c d] e [f g]]" <|
                \_ ->
                    "[a b [c d] e [f g]]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Text_ "b  ", Element_ (Name "c") (EList_ [ Text_ "d " ]), Text_ "  e  ", Element_ (Name "f") (EList_ [ Text_ "g " ]) ]) ]
            , test "abc def [foo] ghi jkl [bar] mno pqr" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl_
                        |> Expect.equal [ Text_ "abc def ", Element_ (Name "foo") (EList_ []), Text_ " ghi jkl ", Element_ (Name "bar") (EList_ []), Text_ " mno pqr" ]
            , test "$a^2$" <|
                \_ ->
                    "$a^2$"
                        |> pl_
                        |> Expect.equal [ Verbatim_ Math "a^2" ]
            , test "`a^2`" <|
                \_ ->
                    "`a^2`"
                        |> pl_
                        |> Expect.equal [ Verbatim_ Code "a^2" ]
            , test "\"a^2\"" <|
                \_ ->
                    "\"a^2\""
                        |> pl_
                        |> Expect.equal [ Verbatim_ Quoted "a^2" ]
            , test "# Images" <|
                \_ ->
                    "# Images"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "heading1") (EList_ [ Text_ "Images" ]) ]
            , test "## Images" <|
                \_ ->
                    "## Images"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "heading2") (EList_ [ Text_ "Images" ]) ]
            , test "[i w:8 p:l https://foo.bar]" <|
                \_ ->
                    "[i w:8 p:l https://foo.bar]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "i") (EList_ [ Text_ "w:8 p:l https://foo.bar" ]) ]
            , test "[a $b$]" <|
                \_ ->
                    "[a $b$]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Verbatim_ Math "b " ]) ]
            , test "abc $x^2$ def" <|
                \_ ->
                    "abc $x^2$ def"
                        |> pl_
                        |> Expect.equal [ Text_ "abc ", Verbatim_ Math "x^2", Text_ " def" ]
            , test "[b L1 GOOD" <|
                \_ ->
                    "[b L1 GOOD"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "error") (Text_ " unmatched '['"), Text_ "b L1 GOOD" ]
            , test "[b L1 is  [i somewhat] like Lisp" <|
                \_ ->
                    "[b L1 is  [i somewhat] like Lisp"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "error") (Text_ " unmatched '['"), Text_ "b L1 is  ", Element_ (Name "i") (EList_ [ Text_ "somewhat" ]), Text_ " like Lisp" ]
            , test "[b aa] $i BB cc" <|
                \_ ->
                    "[b aa] $i BB cc"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "b") (EList_ [ Text_ "aa" ]), Text_ " ", Element_ (Name "error") (Text_ " unmatched '$'"), Text_ "i BB cc" ]
            , test "[a [b c] d]" <|
                \_ ->
                    "[a [b c] d]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Element_ (Name "b") (EList_ [ Text_ "c " ]), Text_ "  d" ]) ]
            , test "[a $b c$ [d e]]" <|
                \_ ->
                    "[a $b c$ [d e]]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Verbatim_ Math "b c ", Text_ "   ", Element_ (Name "d") (EList_ [ Text_ "e " ]) ]) ]
            , test "[a $b$ c]" <|
                \_ ->
                    "[a $b$ c]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "a") (EList_ [ Verbatim_ Math "b ", Text_ "  c" ]) ]
            , test "[image \"A:B\" \"URL\"]" <|
                \_ ->
                    "[image \"A:B\" \"URL\"]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "image") (EList_ [ Verbatim_ Quoted "A:B ", Text_ "   ", Verbatim_ Quoted "URL " ]) ]
            , test "a $b [i c]" <|
                \_ ->
                    "a $b [i c]"
                        |> pl_
                        |> Expect.equal [ Text_ "a ", Element_ (Name "error") (Text_ " unmatched '$'"), Text_ "b ", Element_ (Name "i") (EList_ [ Text_ "c" ]) ]
            , test "AA] BB [link \"CC\" DD] EE" <|
                \_ ->
                    "AA] BB [link \"CC\" DD] EE"
                        |> pl_
                        |> Expect.equal [ Text_ "AA", Element_ (Name "error") (Text_ " unmatched ']'"), Text_ " BB ", Element_ (Name "link") (EList_ [ Verbatim_ Quoted "CC ", Text_ "  DD" ]), Text_ " EE" ]
            ]
