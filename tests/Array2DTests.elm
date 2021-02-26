module Array2DTests exposing (array2dTests)

import Array
import Array2D exposing (Array2D)
import ArrayHelpers
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


array2dTests : Test
array2dTests =
    describe "Array2D"
        [ describe "fromRows"
            [ test "Works correctly on sample input" <|
                \_ ->
                    expectationOn2dArray
                        sample2dArray
                        (\my2dArray ->
                            my2dArray
                                |> Array2D.length
                                |> Expect.equal 12
                        )
            ]
        , describe "fromColumns"
            [ test "Works correctly on sample input" <|
                \_ ->
                    let
                        maybeFromColumns : Maybe (Array2D Int)
                        maybeFromColumns =
                            [ [ 1, 5, 9 ]
                            , [ 2, 6, 10 ]
                            , [ 3, 7, 11 ]
                            , [ 4, 8, 12 ]
                            ]
                                |> ArrayHelpers.list2dToArray2d
                                |> Array2D.fromColumns
                    in
                    maybeFromColumns
                        |> Expect.equal sample2dArray
            ]
        , describe "fromRowMajor"
            [ test "Works correctly on sample input" <|
                \_ ->
                    List.range 1 20
                        |> Array.fromList
                        |> Array2D.fromRowMajor 4 5
                        |> Expect.notEqual Nothing
            ]
        , describe "repeat"
            [ test "Works on sample input" <|
                \_ ->
                    let
                        myArr : Array2D String
                        myArr =
                            Array2D.repeat 3 20 "Hello"
                    in
                    myArr
                        |> Expect.all
                            [ \arr ->
                                arr
                                    |> Array2D.rows
                                    |> Expect.equal 3
                            , \arr ->
                                arr
                                    |> Array2D.columns
                                    |> Expect.equal 20
                            , \arr ->
                                arr
                                    |> Array2D.length
                                    |> Expect.equal 60
                            , \arr ->
                                arr
                                    |> Array2D.get 2 19
                                    |> Expect.equal (Just "Hello")
                            ]
            ]
        , describe "get"
            [ test "Should get the correct element" <|
                \_ ->
                    expectationOn2dArray
                        sample2dArray
                        (\my2dArray ->
                            case Array2D.get 0 2 my2dArray of
                                Just elem ->
                                    elem
                                        |> Expect.equal 3

                                Nothing ->
                                    Expect.fail "Expected to get (Just 3), received Nothing"
                        )
            , test "Should get the correct element 2" <|
                \_ ->
                    expectationOn2dArray
                        sample2dArray
                        (\my2dArray ->
                            case Array2D.get 2 1 my2dArray of
                                Just elem ->
                                    elem
                                        |> Expect.equal 10

                                Nothing ->
                                    Expect.fail "Expected to get (Just 8), received Nothing"
                        )
            , test "Should return Nothing on an out of bounds index" <|
                \_ ->
                    expectationOn2dArray
                        sample2dArray
                        (\my2dArray ->
                            my2dArray
                                |> Array2D.get 0 100
                                |> Expect.equal Nothing
                        )
            ]
        ]


sample2dArray : Maybe (Array2D Int)
sample2dArray =
    [ [ 1, 2, 3, 4 ]
    , [ 5, 6, 7, 8 ]
    , [ 9, 10, 11, 12 ]
    ]
        |> ArrayHelpers.list2dToArray2d
        |> Array2D.fromRows


expectationOn2dArray : Maybe (Array2D a) -> (Array2D a -> Expectation) -> Expectation
expectationOn2dArray maybe2dArray expectFn =
    case maybe2dArray of
        Just my2dArray ->
            expectFn my2dArray

        Nothing ->
            Expect.fail "Should get a Just value here"
