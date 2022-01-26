module UtilsTests exposing (utilsTests)

import Array exposing (Array)
import Expect
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Utils


utilsTests : Test
utilsTests =
    describe "ArrayHelpers"
        [ describe "arrayAll"
            [ test "Passes on good input" <|
                \_ ->
                    List.range 1 5
                        |> Array.fromList
                        |> Utils.arrayAll (\n -> n > 0)
                        |> Expect.true "All of the numbers should be positive"
            , test "Fails on bad input" <|
                \_ ->
                    List.range 0 5
                        |> Array.fromList
                        |> Utils.arrayAll (\n -> n > 0)
                        |> Expect.false "Not all of the numbers are positive"
            ]
        , describe "arrayConcat"
            [ test "Concatenates arrays correctly" <|
                \_ ->
                    let
                        expectedResult : Array Int
                        expectedResult =
                            List.range 1 5
                                |> Array.fromList
                    in
                    [ [ 1, 2 ], [], [ 3, 4 ], [ 5 ], [] ]
                        |> Utils.list2dToArray2d
                        |> Utils.arrayConcat
                        |> Expect.equal expectedResult
            ]
        , describe "indexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (Utils.indexToRowAndColumn 3)
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
                        |> List.map (Utils.indexToRowAndColumn 2)
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
                        |> List.map (Utils.indexToRowAndColumn 4)
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
            , fuzz2 rowsColsFuzzer rowsColsFuzzer "Should work with any number of rows and columns" <|
                \rows cols ->
                    List.range 0 (rows * cols - 1)
                        |> List.map (Utils.indexToRowAndColumn cols)
                        |> List.reverse
                        |> List.head
                        |> Expect.equal (Just ( rows - 1, cols - 1 ))
            ]
        , describe "squareArrayIndexToRowAndColumn"
            [ test "Should map indices and rows/columns correctly" <|
                \_ ->
                    List.range 0 8
                        |> List.map (Utils.squareArrayIndexToRowAndColumn 3)
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
            , fuzz rowsColsFuzzer "Last element indices should both always be one less than the side length" <|
                \sideLength ->
                    let
                        oneLessThanSideLength : Int
                        oneLessThanSideLength =
                            sideLength - 1
                    in
                    List.range 0 ((sideLength ^ 2) - 1)
                        |> List.map (Utils.squareArrayIndexToRowAndColumn sideLength)
                        |> List.reverse
                        |> List.head
                        |> Expect.equal (Just ( oneLessThanSideLength, oneLessThanSideLength ))
            ]
        , describe "firstSubArrayLengthOrZero"
            [ test "Should get the correct length for a basic example" <|
                \_ ->
                    let
                        exampleArray : Array (Array Int)
                        exampleArray =
                            Array.empty
                                |> Array.push (List.range 1 10 |> Array.fromList)
                                |> Array.push Array.empty
                                |> Array.push (List.singleton 5 |> Array.fromList)
                    in
                    exampleArray
                        |> Utils.firstSubArrayLengthOrZero
                        |> Expect.equal 10
            , test "Should get the correct length for a basic example 2" <|
                \_ ->
                    let
                        exampleArray : Array (Array Int)
                        exampleArray =
                            Array.empty
                                |> Array.push (List.singleton 5 |> Array.fromList)
                                |> Array.push Array.empty
                                |> Array.push (List.range 1 10 |> Array.fromList)
                    in
                    exampleArray
                        |> Utils.firstSubArrayLengthOrZero
                        |> Expect.equal 1
            , test "Should get 0 for an empty array" <|
                \_ ->
                    Array.empty
                        |> Utils.firstSubArrayLengthOrZero
                        |> Expect.equal 0
            ]
        ]


rowsColsFuzzer : Fuzzer Int
rowsColsFuzzer =
    intRange 10 500
