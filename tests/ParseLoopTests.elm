module ParseLoopTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Parser.AST exposing (Element__(..), Name(..), VerbatimType(..), simplify)
import L1.Parser.Chunk exposing (pl_)
import Test exposing (..)


suite : Test
suite =
    skip <|
        describe "The Driver.parseLoop function"
            [ test "1. pure text input" <|
                \_ ->
                    "foo"
                        |> pl_
                        |> Expect.equal [ Text__ "foo" ]
            , test "2. simple element" <|
                \_ ->
                    "[foo]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "foo") [] ]
            , test "3. element with two interior pieces" <|
                \_ ->
                    "[i foo]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "i") [ Text__ "foo" ] ]
            , test "4. nested elements" <|
                \_ ->
                    "[i [b foo]]"
                        |> pl_
                        |> Expect.equal [ Element__ (Name "i") [ Element__ (Name "b") [ Text__ "foo " ] ] ]
            , test "5. abc [foo]" <|
                \_ ->
                    "abc [foo]"
                        |> pl_
                        |> Expect.equal [ Text__ "abc ", Element__ (Name "foo") [] ]
            , test "6. simple element preceded and followed by text" <|
                \_ ->
                    "abc [foo] def"
                        |> pl_
                        |> Expect.equal [ Text__ "abc ", Element__ (Name "foo") [], Text__ " def" ]
            , test "7. abc def [foo] ghi jkl [bar] mno pqr" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl_
                        |> Expect.equal
                            [ Text__ "abc def ", Element__ (Name "foo") [], Text__ " ghi jkl ", Element__ (Name "bar") [], Text__ " mno pqr" ]
            , skip <|
                test "8. [x [i a] [j b]]" <|
                    \_ ->
                        "[x [i a] [j b]]"
                            |> pl_
                            |> Expect.equal
                                [ Element__ (Name "x") [ Element__ (Name "i") [ Text__ "a" ], Text__ " ", Element__ (Name "j") [ Text__ "b" ] ] ]
            , test "9. like a list, but with preceding and following text" <|
                \_ ->
                    "abc [x [i a] [j b]] def"
                        |> pl_
                        |> Expect.equal
                            [ Text__ "abc ", Element__ (Name "x") [ Element__ (Name "i") [ Text__ "a" ], Text__ " ", Element__ (Name "j") [ Text__ "b" ] ], Text__ " def" ]
            , only <|
                test "like a list, but with preceding and following text, including newlines" <|
                    \_ ->
                        "abc\n [x [i a] [j b]] \n\ndef"
                            |> pl_
                            |> Expect.equal
                                [ Text__ "abc\n ", Element__ (Name "x") [ Element__ (Name "i") [ Text__ "a" ], Text__ " ", Element__ (Name "j") [ Text__ "b" ] ], Text__ " \n\ndef" ]
            , test "fontRGB (1)" <|
                \_ ->
                    "[fontRGB 255 0 255 foo bar]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "fontRGB") [ Text__ "255", Text__ "0", Text__ "255", Text__ "foo", Text__ "bar" ] ]
            , test "fontRGB (2)" <|
                \_ ->
                    "[fontRGB 255 0 255 foo [b bar]]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "fontRGB") [ Text__ "255", Text__ "0", Text__ "255", Text__ "foo", Element__ (Name "b") [ Text__ "bar" ] ] ]
            , test "fontRGB (3)" <|
                \_ ->
                    "[fontRGB 255 0 255 [i This text is in [b magenta]]]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "fontRGB") [ Text__ "255", Text__ "0", Text__ "255", Element__ (Name "i") [ Text__ "This", Text__ "text", Text__ "is", Text__ "in" ], Element__ (Name "b") [ Text__ "magenta" ] ] ]
            , test "image" <|
                \_ ->
                    "[image caption:Camperdown https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "image") [ Text__ "caption:Camperdown", Text__ "https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg" ] ]
            , test "heading" <|
                \_ ->
                    "# Fault-Tolerant Parsing"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "heading") [ Text__ " Fault-Tolerant Parsing" ] ]
            , test "link (1)" <|
                \_ ->
                    "[link NYT https://nytimes.com]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "link") [ Text__ "NYT", Text__ "https://nytimes.com" ] ]
            , test "link (2)" <|
                \_ ->
                    """[link "Error recovery with parser combinators"  "https://eyalkalderon.com/blog/nom-error-recovery/"]"""
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "link") [ Text__ "Error recovery with parser combinators", Text__ "https://eyalkalderon.com/blog/nom-error-recovery/" ] ]
            , test "code" <|
                \_ ->
                    "`a[0] = 1`"
                        |> pl_
                        |> Expect.equal
                            [ Verbatim__ Code "a[0] = 1" ]
            , test "math" <|
                \_ ->
                    "Pythagoras sez $a^2 + b^2 = c^2$."
                        |> pl_
                        |> Expect.equal
                            [ Text__ "Pythagoras sez ", Verbatim__ Math "a^2 + b^2 = c^2", Text__ "." ]
            , test "chem-physics" <|
                \_ ->
                    "# Introduction to [red Chemistry] [blue Physics]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "heading") [ Text__ " Introduction to ", Element__ (Name "red") [ Text__ "Chemistry" ], Text__ " ", Element__ (Name "blue") [ Text__ "Physics" ] ] ]
            , test "mathblock" <|
                \_ ->
                    "[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "mathblock") [ Text__ "\\int_0^1", Text__ "x^n", Text__ "dx", Text__ "=", Text__ "\\frac{1}{n+1}" ] ]
            , test "[x [y is [b not] good] stuff]" <|
                \_ ->
                    "[x [y is [b not] good] stuff]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "x") [ Element__ (Name "y") [ Text__ "is", Element__ (Name "b") [ Text__ "not" ], Text__ " good" ], Text__ " stuff" ] ]
            , test ":xx [p a] [k b] c" <|
                \_ ->
                    ":xx [p a] [k b] c"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "xx") [ Element__ (Name "p") [ Text__ "a" ], Text__ " ", Element__ (Name "k") [ Text__ "b" ], Text__ " c" ] ]
            , test ":item this is stuff" <|
                \_ ->
                    ":item this is stuff"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "item") [ Text__ "item this is stuff" ] ]
            , test "[red [b [i foo]] [strike This is a test.]" <|
                \_ ->
                    "[red [b [i foo]] [strike This is a test.]"
                        |> pl_
                        |> Expect.equal
                            [ Element__ (Name "error") [ Text__ " unmatched '['" ], Text__ "red ", Element__ (Name "b") [ Element__ (Name "i") [ Text__ "foo " ] ], Text__ " ", Element__ (Name "strike") [ Text__ "This is a test." ] ]
            ]
