module StackReduceTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.AST exposing (Element_(..), Name(..), simplify)
import L1.Check as Check
import Test exposing (..)


suite : Test
suite =
    skip <|
        describe "Stack reducing"
            [ test "1. pure text input" <|
                \_ ->
                    "foo"
                        |> String.length
                        |> Expect.equal 3
            ]
