module Main exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Tests"
        [ Test.todo "write a test here"
        , Test.test "failing test here" <| \_ -> Expect.fail "fail!"
        , Test.test "passing test here" <| \_ -> Expect.pass
        ]
