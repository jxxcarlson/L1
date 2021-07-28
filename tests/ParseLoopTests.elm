module ParseLoopTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Parser.AST exposing (Element_(..), Name(..), VerbatimType(..), simplify)
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
                        |> Expect.equal [ Text_ "foo" ]
            , test "2. simple element" <|
                \_ ->
                    "[foo]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "foo") [] ]
            , test "3. element with two interior pieces" <|
                \_ ->
                    "[i foo]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "i") [ Text_ "foo" ] ]
            , test "4. nested elements" <|
                \_ ->
                    "[i [b foo]]"
                        |> pl_
                        |> Expect.equal [ Element_ (Name "i") [ Element_ (Name "b") [ Text_ "foo " ] ] ]
            , test "5. abc [foo]" <|
                \_ ->
                    "abc [foo]"
                        |> pl_
                        |> Expect.equal [ Text_ "abc ", Element_ (Name "foo") [] ]
            , test "6. simple element preceded and followed by text" <|
                \_ ->
                    "abc [foo] def"
                        |> pl_
                        |> Expect.equal [ Text_ "abc ", Element_ (Name "foo") [], Text_ " def" ]
            , test "7. abc def [foo] ghi jkl [bar] mno pqr" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl_
                        |> Expect.equal
                            [ Text_ "abc def ", Element_ (Name "foo") [], Text_ " ghi jkl ", Element_ (Name "bar") [], Text_ " mno pqr" ]
            , skip <|
                test "8. [x [i a] [j b]]" <|
                    \_ ->
                        "[x [i a] [j b]]"
                            |> pl_
                            |> Expect.equal
                                [ Element_ (Name "x") [ Element_ (Name "i") [ Text_ "a" ], Text_ " ", Element_ (Name "j") [ Text_ "b" ] ] ]
            , test "9. like a list, but with preceding and following text" <|
                \_ ->
                    "abc [x [i a] [j b]] def"
                        |> pl_
                        |> Expect.equal
                            [ Text_ "abc ", Element_ (Name "x") [ Element_ (Name "i") [ Text_ "a" ], Text_ " ", Element_ (Name "j") [ Text_ "b" ] ], Text_ " def" ]
            , only <|
                test "like a list, but with preceding and following text, including newlines" <|
                    \_ ->
                        "abc\n [x [i a] [j b]] \n\ndef"
                            |> pl_
                            |> Expect.equal
                                [ Text_ "abc\n ", Element_ (Name "x") [ Element_ (Name "i") [ Text_ "a" ], Text_ " ", Element_ (Name "j") [ Text_ "b" ] ], Text_ " \n\ndef" ]
            , test "fontRGB (1)" <|
                \_ ->
                    "[fontRGB 255 0 255 foo bar]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "fontRGB") [ Text_ "255", Text_ "0", Text_ "255", Text_ "foo", Text_ "bar" ] ]
            , test "fontRGB (2)" <|
                \_ ->
                    "[fontRGB 255 0 255 foo [b bar]]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "fontRGB") [ Text_ "255", Text_ "0", Text_ "255", Text_ "foo", Element_ (Name "b") [ Text_ "bar" ] ] ]
            , test "fontRGB (3)" <|
                \_ ->
                    "[fontRGB 255 0 255 [i This text is in [b magenta]]]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "fontRGB") [ Text_ "255", Text_ "0", Text_ "255", Element_ (Name "i") [ Text_ "This", Text_ "text", Text_ "is", Text_ "in" ], Element_ (Name "b") [ Text_ "magenta" ] ] ]
            , test "image" <|
                \_ ->
                    "[image caption:Camperdown https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "image") [ Text_ "caption:Camperdown", Text_ "https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg" ] ]
            , test "heading" <|
                \_ ->
                    "# Fault-Tolerant Parsing"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "heading") [ Text_ " Fault-Tolerant Parsing" ] ]
            , test "link (1)" <|
                \_ ->
                    "[link NYT https://nytimes.com]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "link") [ Text_ "NYT", Text_ "https://nytimes.com" ] ]
            , test "link (2)" <|
                \_ ->
                    """[link "Error recovery with parser combinators"  "https://eyalkalderon.com/blog/nom-error-recovery/"]"""
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "link") [ Text_ "Error recovery with parser combinators", Text_ "https://eyalkalderon.com/blog/nom-error-recovery/" ] ]
            , test "code" <|
                \_ ->
                    "`a[0] = 1`"
                        |> pl_
                        |> Expect.equal
                            [ Verbatim_ Code "a[0] = 1" ]
            , test "math" <|
                \_ ->
                    "Pythagoras sez $a^2 + b^2 = c^2$."
                        |> pl_
                        |> Expect.equal
                            [ Text_ "Pythagoras sez ", Verbatim_ Math "a^2 + b^2 = c^2", Text_ "." ]
            , test "chem-physics" <|
                \_ ->
                    "# Introduction to [red Chemistry] [blue Physics]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "heading") [ Text_ " Introduction to ", Element_ (Name "red") [ Text_ "Chemistry" ], Text_ " ", Element_ (Name "blue") [ Text_ "Physics" ] ] ]
            , test "mathblock" <|
                \_ ->
                    "[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "mathblock") [ Text_ "\\int_0^1", Text_ "x^n", Text_ "dx", Text_ "=", Text_ "\\frac{1}{n+1}" ] ]
            , test "[x [y is [b not] good] stuff]" <|
                \_ ->
                    "[x [y is [b not] good] stuff]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "x") [ Element_ (Name "y") [ Text_ "is", Element_ (Name "b") [ Text_ "not" ], Text_ " good" ], Text_ " stuff" ] ]
            , test ":xx [p a] [k b] c" <|
                \_ ->
                    ":xx [p a] [k b] c"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "xx") [ Element_ (Name "p") [ Text_ "a" ], Text_ " ", Element_ (Name "k") [ Text_ "b" ], Text_ " c" ] ]
            , test ":item this is stuff" <|
                \_ ->
                    ":item this is stuff"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "item") [ Text_ "item this is stuff" ] ]
            , test "[red [b [i foo]] [strike This is a test.]" <|
                \_ ->
                    "[red [b [i foo]] [strike This is a test.]"
                        |> pl_
                        |> Expect.equal
                            [ Element_ (Name "error") [ Text_ " unmatched '['" ], Text_ "red ", Element_ (Name "b") [ Element_ (Name "i") [ Text_ "foo " ] ], Text_ " ", Element_ (Name "strike") [ Text_ "This is a test." ] ]
            ]
