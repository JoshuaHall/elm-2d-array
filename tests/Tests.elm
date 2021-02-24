module Tests exposing (array2dTests, arrayHelpersTests)

import Array
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
                            case Array2D.get 0 0 my2dArray of
                                Just elem ->
                                    elem
                                        |> Expect.equal 3

                                Nothing ->
                                    Expect.fail "Expected to get (Just 3), received Nothing"
                        )
            ]
        ]


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
                        expectedResult =
                            [ 1, 2, 3, 4, 5 ]
                                |> Array.fromList
                    in
                    [ [ 1, 2 ], [ 3, 4 ], [ 5 ], [] ]
                        |> List.map Array.fromList
                        |> Array.fromList
                        |> arrayConcat
                        |> Expect.equal expectedResult
            ]
        ]
