module DifferTests exposing (..)

-- http://package.elm-lang.org/packages/elm-community/elm-test/latest

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1.Library.Differ exposing (..)
import Test exposing (..)


p1 =
    [ "red", "green", "blue" ]


p2 =
    [ "red", "verde", "blue" ]


r1 =
    List.map String.toUpper p1


suite : Test
suite =
    skip <|
        describe "Differ"
            -- Nest as many descriptions as you like.
            [ test "(0) compute diff of p1 and p1 " <|
                \_ ->
                    let
                        diffRecord =
                            diff p1 p1
                    in
                    Expect.equal diffRecord.middleSegmentInTarget []
            , test "(1) compute diff of p1 and p2 " <|
                \_ ->
                    let
                        diffRecord =
                            diff p1 p2
                    in
                    Expect.equal diffRecord.middleSegmentInTarget [ "verde" ]
            , test "(2) compute the differential transform for diff p1 p2 and r1" <|
                \_ ->
                    let
                        diffRecord =
                            diff p1 p2

                        r2 =
                            differentialTransform String.toUpper diffRecord r1
                    in
                    Expect.equal r2 [ "RED", "VERDE", "BLUE" ]
            ]
