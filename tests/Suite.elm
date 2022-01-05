module Suite exposing (testSuite)

import Array2DTests exposing (array2dTests)
import SquareArray2DTests exposing (squareArray2dTests)
import Test exposing (Test, describe)
import UtilsTests exposing (utilsTests)


testSuite : Test
testSuite =
    describe "elm-2d-array Test Suite"
        [ array2dTests
        , squareArray2dTests
        , utilsTests
        ]
