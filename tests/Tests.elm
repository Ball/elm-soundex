module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

all : Test
all = describe "Sample Test Suite"
    [ describe "Unit test examples"
        [ test "Addition" <|
            \() ->
                Expect.equal (3 + 7) 10
        , test "String.left" <|
            \() -> Expect.equal "a" (String.left 1 "abcdefg")
        ]
    , describe "Fuzz Sample Testing"
        [ fuzz (list int) "Lists always have positive length" <|
            \aList -> List.length aList |> Expect.atLeast 0
        , fuzz (list int) "Sorting doesn't chnage its length" <|
            \aList -> List.sort aList |> List.length |> Expect.equal (List.length aList)
        , fuzzWith {runs = 1000} int "List.member will find an integer in a list containing it" <|
            \i ->
                List.member i [i] |> Expect.true "If you see this, List.member returned False!"
        , fuzz2 string string "The length of a string is equal to the sum of it's substrings' lengths" <|
            \s1 s2 ->
                s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
        ]
    ]