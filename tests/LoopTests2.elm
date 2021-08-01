module LoopTests2 exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Parser.AST exposing (Element__(..), Name(..), VerbatimType(..), simplify)
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
                        |> Expect.equal [ Text__ "a" ]
            , test "[a] b" <|
                \_ ->
                    "[a] b"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [], Text__ " b" ]
            , test "[a b]" <|
                \_ ->
                    "[a b]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Text__ "b" ] ]
            , test "a [b c]" <|
                \_ ->
                    "a [b c]"
                        |> pl_
                        |> Expect.equal [ Text__ "a ", Element__ (Name "b") [ Text__ "c" ] ]
            , test "[a b] c" <|
                \_ ->
                    "[a b] c"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Text__ "b" ], Text__ " c" ]
            , test "[a b] [c d]" <|
                \_ ->
                    "[a b] [c d]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Text__ "b" ], Text__ " ", Element__ (Name "c") [ Text__ "d" ] ]
            , test "[a b] x [c d]" <|
                \_ ->
                    "[a b] x [c d]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Text__ "b" ], Text__ " x ", Element__ (Name "c") [ Text__ "d" ] ]
            , test "u [a b] x [c d]" <|
                \_ ->
                    "u [a b] x [c d]"
                        |> pl_
                        |> Expect.equal [ Text__ "u ", Element__ (Name "a") [ Text__ "b" ], Text__ " x ", Element__ (Name "c") [ Text__ "d" ] ]
            , test "[x [a b]]" <|
                \_ ->
                    "[x [a b]]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "x") [ Element__ (Name "a") [ Text__ "b " ] ] ]
            , test "a b [c d] e [f g]" <|
                \_ ->
                    "a b [c d] e [f g]"
                        |> pl_
                        |> Expect.equal [ Text__ "a b ", Element__ (Name "c") [ Text__ "d" ], Text__ " e ", Element__ (Name "f") [ Text__ "g" ] ]
            , test "[a b [c d] e [f g]]" <|
                \_ ->
                    "[a b [c d] e [f g]]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Text__ "b  ", Element__ (Name "c") [ Text__ "d " ], Text__ "  e  ", Element__ (Name "f") [ Text__ "g " ] ] ]
            , test "abc def [foo] ghi jkl [bar] mno pqr" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl_
                        |> Expect.equal [ Text__ "abc def ", Element__ (Name "foo") [], Text__ " ghi jkl ", Element__ (Name "bar") [], Text__ " mno pqr" ]
            , test "$a^2$" <|
                \_ ->
                    "$a^2$"
                        |> pl_
                        |> Expect.equal [ Verbatim__ Math "a^2" ]
            , test "`a^2`" <|
                \_ ->
                    "`a^2`"
                        |> pl_
                        |> Expect.equal [ Verbatim__ Code "a^2" ]
            , test "\"a^2\"" <|
                \_ ->
                    "\"a^2\""
                        |> pl_
                        |> Expect.equal [ Verbatim__ Quoted "a^2" ]
            , test "# Images" <|
                \_ ->
                    "# Images"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "heading1") [ Text__ "Images" ] ]
            , test "## Images" <|
                \_ ->
                    "## Images"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "heading2") [ Text__ "Images" ] ]
            , test "[i w:8 p:l https://foo.bar]" <|
                \_ ->
                    "[i w:8 p:l https://foo.bar]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "i") [ Text__ "w:8 p:l https://foo.bar" ] ]
            , test "[a $b$]" <|
                \_ ->
                    "[a $b$]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Verbatim__ Math "b " ] ]
            , test "abc $x^2$ def" <|
                \_ ->
                    "abc $x^2$ def"
                        |> pl_
                        |> Expect.equal [ Text__ "abc ", Verbatim__ Math "x^2", Text__ " def" ]
            , test "[b L1 GOOD" <|
                \_ ->
                    "[b L1 GOOD"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "error") [ Text__ " unmatched '['" ], Text__ "b L1 GOOD" ]
            , test "[b L1 is  [i somewhat] like Lisp" <|
                \_ ->
                    "[b L1 is  [i somewhat] like Lisp"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "error") [ Text__ " unmatched '['" ], Text__ "b L1 is  ", Element__ (Name "i") [ Text__ "somewhat" ], Text__ " like Lisp" ]
            , test "[b aa] $i BB cc" <|
                \_ ->
                    "[b aa] $i BB cc"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "b") [ Text__ "aa" ], Text__ " ", Element__ (Name "error") [ Text__ " unmatched '$'" ], Text__ "i BB cc" ]
            , test "[a [b c] d]" <|
                \_ ->
                    "[a [b c] d]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Element__ (Name "b") [ Text__ "c " ], Text__ "  d" ] ]
            , test "[a $b c$ [d e]]" <|
                \_ ->
                    "[a $b c$ [d e]]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Verbatim__ Math "b c ", Text__ "   ", Element__ (Name "d") [ Text__ "e " ] ] ]
            , test "[a $b$ c]" <|
                \_ ->
                    "[a $b$ c]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "a") [ Verbatim__ Math "b ", Text__ "  c" ] ]
            , test "[image \"A:B\" \"URL\"]" <|
                \_ ->
                    "[image \"A:B\" \"URL\"]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "image") [ Verbatim__ Quoted "A:B ", Text__ "   ", Verbatim__ Quoted "URL " ] ]
            , test "a $b [i c]" <|
                \_ ->
                    "a $b [i c]"
                        |> pl_
                        |> Expect.equal [ Text__ "a ", Element__ (Name "error") [ Text__ " unmatched '$'" ], Text__ "b ", Element__ (Name "i") [ Text__ "c" ] ]
            , test "AA] BB [link \"CC\" DD] EE" <|
                \_ ->
                    "AA] BB [link \"CC\" DD] EE"
                        |> pl_
                        |> Expect.equal [ Text__ "AA", Element__ (Name "error") [ Text__ " unmatched ']'" ], Text__ " BB ", Element__ (Name "link") [ Verbatim__ Quoted "CC ", Text__ "  DD" ], Text__ " EE" ]
            ]
