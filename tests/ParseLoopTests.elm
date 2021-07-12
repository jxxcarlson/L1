module ParseLoopTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser.AST exposing (Element_(..), Name(..), simplify)
import Parser.Driver exposing (pl)
import Test exposing (..)


suite : Test
suite =
    describe "The Driver.parseLoop function"
        [ test "pure text input" <|
            \_ ->
                "foo"
                    |> pl
                    |> Expect.equal [ Text_ "foo" ]
        , only <|
            test "simple element" <|
                \_ ->
                    "[foo]"
                        |> pl
                        |> Expect.equal [ Element_ (Name "foo") (EList_ []) ]
        , test "element with two interior pieces" <|
            \_ ->
                "[i foo]"
                    |> pl
                    |> Expect.equal [ Element_ (Name "i") (EList_ [ Text_ "foo" ]) ]
        , test "nested elements" <|
            \_ ->
                "[i [b foo]]"
                    |> pl
                    --|> Expect.equal [Element_ (Name ("i ")) [] (Element_ (Name "b") [] (EList_ [Raw_ "foo"]))]
                    |> Expect.equal [ Element_ (Name "i") (EList_ [ Element_ (Name "b") (EList_ [ Text_ "foo" ]) ]) ]
        , test "simple element preceded by text" <|
            \_ ->
                "abc [foo]"
                    |> pl
                    |> Expect.equal [ Text_ "abc ", Element_ (Name "foo") (EList_ []) ]
        , test "simple element preceded and followed by text" <|
            \_ ->
                "abc [foo] def"
                    |> pl
                    |> Expect.equal [ Text_ "abc ", Element_ (Name "foo") (EList_ []), Text_ " def" ]
        , test "simple element preceded and followed by text (2)" <|
            \_ ->
                "abc def [foo] ghi jkl [bar] mno pqr"
                    |> pl
                    |> Expect.equal
                        [ Text_ "abc def ", Element_ (Name "foo") (EList_ []), Text_ " ghi jkl ", Element_ (Name "bar") (EList_ []), Text_ " mno pqr" ]
        , test "like a list" <|
            \_ ->
                "[x [i a] [j b]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "x") (EList_ [ Element_ (Name "i") (EList_ [ Text_ "a" ]), Text_ " ", Element_ (Name "j") (EList_ [ Text_ "b" ]) ]) ]
        , test "like a list, but with preceding and following text" <|
            \_ ->
                "abc [x [i a] [j b]] def"
                    |> pl
                    |> Expect.equal
                        [ Text_ "abc ", Element_ (Name "x") (EList_ [ Element_ (Name "i") (EList_ [ Text_ "a" ]), Text_ " ", Element_ (Name "j") (EList_ [ Text_ "b" ]) ]), Text_ " def" ]
        , test "like a list, but with preceding and following text, including newlines" <|
            \_ ->
                "abc\n [x [i a] [j b]] \n\ndef"
                    |> pl
                    |> Expect.equal
                        [ Text_ "abc\n ", Element_ (Name "x") (EList_ [ Element_ (Name "i") (EList_ [ Text_ "a" ]), Text_ " ", Element_ (Name "j") (EList_ [ Text_ "b" ]) ]), Text_ " \n\ndef" ]
        , test "fontRGB (1)" <|
            \_ ->
                "[fontRGB 255 0 255 foo bar]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "fontRGB") (EList_ [ Text_ "255", Text_ "0", Text_ "255", Text_ "foo", Text_ "bar" ]) ]
        , test "fontRGB (2)" <|
            \_ ->
                "[fontRGB 255 0 255 foo [b bar]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "fontRGB") (EList_ [ Text_ "255", Text_ "0", Text_ "255", Text_ "foo", Element_ (Name "b") (EList_ [ Text_ "bar" ]) ]) ]
        , test "fontRGB (3)" <|
            \_ ->
                "[fontRGB 255 0 255 [i This text is in [b magenta]]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "fontRGB") (EList_ [ Text_ "255", Text_ "0", Text_ "255", Element_ (Name "i") (EList_ [ Text_ "This", Text_ "text", Text_ "is", Text_ "in" ]), Element_ (Name "b") (EList_ [ Text_ "magenta" ]) ]) ]
        , test "image" <|
            \_ ->
                "[image caption:Camperdown https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "image") (EList_ [ Text_ "caption:Camperdown", Text_ "https://upload.wikimedia.org/wikipedia/commons/2/20/Camperdown_Elm_Prospect_Park_Brooklyn.jpg" ]) ]
        , test "heading" <|
            \_ ->
                "# Fault-Tolerant Parsing"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "heading") (EList_ [ Text_ " Fault-Tolerant Parsing" ]) ]
        , skip <|
            test "link (1)" <|
                \_ ->
                    "[link NYT https://nytimes.com]"
                        |> pl
                        |> Expect.equal
                            [ Element_ (Name "link") (EList_ [ Text_ "NYT", Text_ "https://nytimes.com" ]) ]
        , skip <|
            test "link (2)" <|
                \_ ->
                    """[link "Error recovery with parser combinators"  "https://eyalkalderon.com/blog/nom-error-recovery/"]"""
                        |> pl
                        |> Expect.equal
                            [ Element_ (Name "link") (EList_ [ Text_ "Error recovery with parser combinators", Text_ "https://eyalkalderon.com/blog/nom-error-recovery/" ]) ]
        , test "[x [i a] [j b]]" <|
            \_ ->
                "[x [i a] [j b]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "x") (EList_ [ Element_ (Name "i") (EList_ [ Text_ "a" ]), Text_ " ", Element_ (Name "j") (EList_ [ Text_ "b" ]) ]) ]
        , test "code" <|
            \_ ->
                "`a[0] = 1`"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "code") (Text_ "a[0] = 1") ]
        , test "math" <|
            \_ ->
                "Pythagoras sez $a^2 + b^2 = c^2$."
                    |> pl
                    |> Expect.equal
                        [ Text_ "Pythagoras sez ", Element_ (Name "math2") (Text_ "a^2 + b^2 = c^2"), Text_ "." ]
        , test "chem-physics" <|
            \_ ->
                "# Introduction to [red Chemistry] [blue Physics]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "heading") (EList_ [ Text_ " Introduction to ", Element_ (Name "red") (EList_ [ Text_ "Chemistry" ]), Text_ " ", Element_ (Name "blue") (EList_ [ Text_ "Physics" ]) ]) ]
        , test "mathblock" <|
            \_ ->
                "[mathblock \\int_0^1 x^n dx = \\frac{1}{n+1}]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "mathblock") (EList_ [ Text_ "\\int_0^1", Text_ "x^n", Text_ "dx", Text_ "=", Text_ "\\frac{1}{n+1}" ]) ]
        , test "[x [y is [b not] good] stuff]" <|
            \_ ->
                "[x [y is [b not] good] stuff]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "x") (EList_ [ Element_ (Name "y") (EList_ [ Text_ "is", Element_ (Name "b") (EList_ [ Text_ "not" ]), Text_ " good" ]), Text_ " stuff" ]) ]
        ]
