module Suite exposing (testSuite)

import Array2DTests exposing (array2dTests)
import ArrayHelpersTests exposing (arrayHelpersTests)
import SquareArray2DTests exposing (squareArray2dTests)
import Test exposing (Test, describe)


testSuite : Test
testSuite =
    describe "elm-2d-array Test Suite"
        [ array2dTests
        , squareArray2dTests
        , arrayHelpersTests
        ]
