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
                    |> Expect.equal [ Raw_ "foo" ]
        , test "simple element" <|
            \_ ->
                "[foo]"
                    |> pl
                    |> Expect.equal [ Element_ (Name "foo") [] (EList_ []) ]
        , test "element with two interior pieces" <|
            \_ ->
                "[i foo]"
                    |> pl
                    |> Expect.equal [ Element_ (Name "i") [] (EList_ [ Raw_ "foo" ]) ]
        , test "nested elements" <|
            \_ ->
                "[i [b foo]]"
                    |> pl
                    --|> Expect.equal [Element_ (Name ("i ")) [] (Element_ (Name "b") [] (EList_ [Raw_ "foo"]))]
                    |> Expect.equal [ Element_ (Name "i") [] (EList_ [ Element_ (Name "b") [] (EList_ [ Raw_ "foo" ]) ]) ]
        , test "simple element preceded by text" <|
            \_ ->
                "abc [foo]"
                    |> pl
                    |> Expect.equal [ Raw_ "abc ", Element_ (Name "foo") [] (EList_ []) ]
        , test "simple element preceded and followed by text" <|
            \_ ->
                "abc [foo] def"
                    |> pl
                    |> Expect.equal [ Raw_ "abc ", Element_ (Name "foo") [] (EList_ []), Raw_ " def" ]
        , test "simple element preceded and followed by text (2)" <|
            \_ ->
                "abc def [foo] ghi jkl [bar] mno pqr"
                    |> pl
                    |> Expect.equal
                        [ Raw_ "abc def ", Element_ (Name "foo") [] (EList_ []), Raw_ " ghi jkl ", Element_ (Name "bar") [] (EList_ []), Raw_ " mno pqr" ]
        , test "like a list" <|
            \_ ->
                "[x [i a] [j b]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "x") [] (EList_ [ Element_ (Name "i") [] (EList_ [ Raw_ "a" ]), Element_ (Name "j") [] (EList_ [ Raw_ "b" ]) ]) ]
        , test "like a list, but with preceding and following text" <|
            \_ ->
                "abc [x [i a] [j b]] def"
                    |> pl
                    |> Expect.equal
                        [ Raw_ "abc ", Element_ (Name "x") [] (EList_ [ Element_ (Name "i") [] (EList_ [ Raw_ "a" ]), Element_ (Name "j") [] (EList_ [ Raw_ "b" ]) ]), Raw_ " def" ]
        , test "like a list, but with preceding and following text, including newlines" <|
            \_ ->
                "abc\n [x [i a] [j b]] \n\ndef"
                    |> pl
                    |> Expect.equal
                        [ Raw_ "abc\n ", Element_ (Name "x") [] (EList_ [ Element_ (Name "i") [] (EList_ [ Raw_ "a" ]), Element_ (Name "j") [] (EList_ [ Raw_ "b" ]) ]), Raw_ " \n\ndef" ]
        , test "fontRGB" <|
            \_ ->
                "[fontRGB 255 0 255 foo [b bar]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "fontRGB") [] (EList_ [ Raw_ "255 ", Raw_ "0 ", Raw_ "255 ", Raw_ "foo ", Element_ (Name "b") [] (EList_ [ Raw_ "bar" ]) ]) ]
        , test "fontRGB (2)" <|
            \_ ->
                "[fontRGB 255 0 255 [i This text is in [b magenta]]]"
                    |> pl
                    |> Expect.equal
                        [ Element_ (Name "fontRGB") [] (EList_ [ Raw_ "255 ", Raw_ "0 ", Raw_ "255 ", Element_ (Name "i") [] (EList_ [ Raw_ "This ", Raw_ "text ", Raw_ "is ", Raw_ "in ", Element_ (Name "b") [] (EList_ [ Raw_ "magenta" ]) ]) ]) ]
        ]



-- "[fontRGB 255 0 255 [i This text is in [b magenta]]]"
-- [fontRGB 255 0 255 foo [b bar]]
-- [[Raw_ (" ghi jkl"),Element_ (Name "foo") [] (EList_ []),Raw_ ("abc def ")]
