module Tests exposing (..)

import Test exposing (..)
import Expect


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Test" <| \() -> Expect.equal True True
            ]
        ]
