module Tests exposing (array2dTests, arrayHelpersTests, squareArray2dTests)

import Array exposing (Array)
import Array2D exposing (Array2D)
import ArrayHelpers exposing (arrayAll, arrayConcat)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


sample2dArray : Maybe (Array2D Int)
sample2dArray =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    ]
        |> List.map Array.fromList
        |> Array.fromList
        |> Array2D.fromRows


expectationOn2dArray : Maybe (Array2D a) -> (Array2D a -> Expectation) -> Expectation
expectationOn2dArray maybe2dArray expectFn =
    case maybe2dArray of
        Just my2dArray ->
            expectFn my2dArray

        Nothing ->
            Expect.fail "Should get a Just value here"



{- Internal copies -}


indexToRowAndColumn2D_ARRAY : Int -> Int -> Int -> ( Int, Int )
indexToRowAndColumn2D_ARRAY numRows numColumns index =
    ( index // numRows, modBy numColumns index )


indexToRowAndColumnSQUARE_2D_ARRAY : Int -> Int -> ( Int, Int )
indexToRowAndColumnSQUARE_2D_ARRAY sideLength index =
    ( index // sideLength, modBy sideLength index )


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
                                |> Array2D.numElements
                                |> Expect.equal 9
                        )
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
                                        |> Expect.equal 8

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
        , describe "indexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (indexToRowAndColumn2D_ARRAY 3 3)
                        |> Expect.equalLists
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            ]
            ]
        ]


squareArray2dTests : Test
squareArray2dTests =
    describe "SquareArray2D"
        [ describe "indexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (indexToRowAndColumnSQUARE_2D_ARRAY 3)
                        |> Expect.equalLists
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            ]
            ]
        ]



-- [ describe "fromRows"
--     [ test "Works correctly on sample input" <|
--         \_ ->
--             expectationOn2dArray
--                 sample2dArray
--                 (\my2dArray ->
--                     my2dArray
--                         |> Array2D.numElements
--                         |> Expect.equal 9
--                 )
--     ]
-- , describe "get"
--     [ test "Should get the correct element" <|
--         \_ ->
--             expectationOn2dArray
--                 sample2dArray
--                 (\my2dArray ->
--                     case Array2D.get 0 2 my2dArray of
--                         Just elem ->
--                             elem
--                                 |> Expect.equal 3
--                         Nothing ->
--                             Expect.fail "Expected to get (Just 3), received Nothing"
--                 )
--     , test "Should get the correct element 2" <|
--         \_ ->
--             expectationOn2dArray
--                 sample2dArray
--                 (\my2dArray ->
--                     case Array2D.get 2 1 my2dArray of
--                         Just elem ->
--                             elem
--                                 |> Expect.equal 8
--                         Nothing ->
--                             Expect.fail "Expected to get (Just 8), received Nothing"
--                 )
--     , test "Should return Nothing on an out of bounds index" <|
--         \_ ->
--             expectationOn2dArray
--                 sample2dArray
--                 (\my2dArray ->
--                     my2dArray
--                         |> Array2D.get 0 100
--                         |> Expect.equal Nothing
--                 )
--     ]
-- ]


arrayHelpersTests : Test
arrayHelpersTests =
    describe "ArrayHelpers"
        [ describe "arrayAll"
            [ test "Passes on good input" <|
                \_ ->
                    [ 1, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> arrayAll (\n -> n > 0)
                        |> Expect.true "Expected to pass"
            , test "Fails on bad input" <|
                \_ ->
                    [ 0, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> arrayAll (\n -> n > 0)
                        |> Expect.false "Expected to pass"
            ]
        , describe "arrayConcat"
            [ test "Concatenates arrays correctly" <|
                \_ ->
                    let
                        expectedResult : Array Int
                        expectedResult =
                            [ 1, 2, 3, 4, 5 ]
                                |> Array.fromList
                    in
                    [ [ 1, 2 ], [], [ 3, 4 ], [ 5 ], [] ]
                        |> List.map Array.fromList
                        |> Array.fromList
                        |> arrayConcat
                        |> Expect.equal expectedResult
            ]
        ]
