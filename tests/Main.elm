module Main exposing (..)

import Test exposing (..)
import Test.Runner.Html
import Fuzz exposing (..)
import Expect
import PostgRest as PG


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
