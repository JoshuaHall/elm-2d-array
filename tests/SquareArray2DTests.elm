module SquareArray2DTests exposing (squareArray2dTests)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (intRange)
import SquareArray2D exposing (SquareArray2D)
import Test exposing (Test, describe, fuzz2, test)
import Utils


squareArray2dTests : Test
squareArray2dTests =
    describe "SquareArray2D"
        [ describe "fromRows"
            [ test "Works correctly on sample input" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\mySquare2dArray ->
                            mySquare2dArray
                                |> SquareArray2D.length
                                |> Expect.equal 9
                        )
            ]
        , describe "fromColumns"
            [ test "Works correctly on sample input" <|
                \_ ->
                    [ [ 1, 4, 7 ]
                    , [ 2, 5, 8 ]
                    , [ 3, 6, 9 ]
                    ]
                        |> Utils.list2dToArray2d
                        |> SquareArray2D.fromColumns
                        |> Expect.equal sampleSquare2dArray
            ]
        , describe "fromRowMajor"
            [ test "Works correctly on sample input" <|
                \_ ->
                    List.range 1 36
                        |> Array.fromList
                        |> SquareArray2D.fromRowMajor 6
                        |> Expect.notEqual Nothing
            ]
        , describe "repeat"
            [ test "Works on sample input" <|
                \_ ->
                    let
                        myArr : SquareArray2D String
                        myArr =
                            SquareArray2D.repeat 20 "Hello"
                    in
                    myArr
                        |> Expect.all
                            [ \arr ->
                                arr
                                    |> SquareArray2D.sideLength
                                    |> Expect.equal 20
                            , \arr ->
                                arr
                                    |> SquareArray2D.length
                                    |> Expect.equal 400
                            , \arr ->
                                arr
                                    |> SquareArray2D.get 2 19
                                    |> Expect.equal (Just "Hello")
                            ]
            ]
        , describe "get"
            [ test "Should get the correct element" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\mySquare2dArray ->
                            case SquareArray2D.get 0 2 mySquare2dArray of
                                Just elem ->
                                    elem
                                        |> Expect.equal 3

                                Nothing ->
                                    Expect.fail "Expected to get (Just 3), received Nothing"
                        )
            , test "Should get the correct element 2" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\mySquare2dArray ->
                            case SquareArray2D.get 2 1 mySquare2dArray of
                                Just elem ->
                                    elem
                                        |> Expect.equal 8

                                Nothing ->
                                    Expect.fail "Expected to get (Just 8), received Nothing"
                        )
            , test "Should return Nothing on an out of bounds index" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\mySquare2dArray ->
                            mySquare2dArray
                                |> SquareArray2D.get 0 100
                                |> Expect.equal Nothing
                        )
            , test "Should return Nothing on an out of bounds index 2" <|
                \_ ->
                    expectationOnSquare2dArray
                        (SquareArray2D.fromRows <|
                            Array.fromList
                                [ Array.fromList [ 1, 2 ]
                                , Array.fromList [ 3, 4 ]
                                ]
                        )
                        (\my2dArray ->
                            my2dArray
                                |> SquareArray2D.get 1 -1
                                |> Expect.equal Nothing
                        )
            ]
        , describe "set"
            [ fuzz2 (intRange 0 2) (intRange 0 2) "Should correctly set an element" <|
                \row column ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\my2dArray ->
                            my2dArray
                                |> SquareArray2D.set row column 54100
                                |> SquareArray2D.get row column
                                |> Expect.equal (Just 54100)
                        )
            , test "Does nothing if the indices are out of bounds" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\my2dArray ->
                            my2dArray
                                |> SquareArray2D.set 2000 2000 54100
                                |> Expect.equal my2dArray
                        )
            ]
        , describe "map"
            [ test "Should correctly map elements" <|
                \_ ->
                    expectationOnSquare2dArray
                        sampleSquare2dArray
                        (\my2dArray ->
                            my2dArray
                                |> SquareArray2D.map String.fromInt
                                |> SquareArray2D.get 1 1
                                |> Expect.equal (Just "5")
                        )
            ]
        ]


sampleSquare2dArray : Maybe (SquareArray2D Int)
sampleSquare2dArray =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    ]
        |> Utils.list2dToArray2d
        |> SquareArray2D.fromRows


expectationOnSquare2dArray : Maybe (SquareArray2D a) -> (SquareArray2D a -> Expectation) -> Expectation
expectationOnSquare2dArray maybeSquare2dArray expectFn =
    case maybeSquare2dArray of
        Just mySquare2dArray ->
            expectFn mySquare2dArray

        Nothing ->
            Expect.fail "Should get a Just value here"
