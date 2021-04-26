module ArrayHelpersTests exposing (arrayHelpersTests)

import Array exposing (Array)
import ArrayHelpers
import Expect
import Test exposing (Test, describe, test)


arrayHelpersTests : Test
arrayHelpersTests =
    describe "ArrayHelpers"
        [ describe "arrayAll"
            [ test "Passes on good input" <|
                \_ ->
                    [ 1, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> ArrayHelpers.arrayAll (\n -> n > 0)
                        |> Expect.true "Expected to pass"
            , test "Fails on bad input" <|
                \_ ->
                    [ 0, 2, 3, 4, 5 ]
                        |> Array.fromList
                        |> ArrayHelpers.arrayAll (\n -> n > 0)
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
                        |> ArrayHelpers.list2dToArray2d
                        |> ArrayHelpers.arrayConcat
                        |> Expect.equal expectedResult
            ]
        , describe "indexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (ArrayHelpers.indexToRowAndColumn 3)
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
            , test "Should work with 4 rows and 2 columns" <|
                \_ ->
                    List.range 0 7
                        |> List.map (ArrayHelpers.indexToRowAndColumn 2)
                        |> Expect.equalLists
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 3, 0 )
                            , ( 3, 1 )
                            ]
            , test "Should work with 2 rows and 4 columns" <|
                \_ ->
                    List.range 0 7
                        |> List.map (ArrayHelpers.indexToRowAndColumn 4)
                        |> Expect.equalLists
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 1, 3 )
                            ]
            ]
        , describe "squareArrayIndexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (ArrayHelpers.squareArrayIndexToRowAndColumn 3)
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
