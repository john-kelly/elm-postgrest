module Main exposing (..)

import Tests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    [ tests
    ]
        |> concat
        |> Test.Runner.Html.run


tests : Test
tests =
    describe "tests"
        [ test "the empty list has 0 length" <|
            \() -> List.length [] |> Expect.equal 0
        ]
