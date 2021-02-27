module Benchmarks exposing (suite)

import Array exposing (Array)
import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SquareArray2D exposing (SquareArray2D)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        length : Int
        length =
            100

        arraySample : Array (Array Bool)
        arraySample =
            let
                subArray =
                    Array.repeat length False
            in
            Array.repeat
                length
                subArray

        array2dSample : SquareArray2D Bool
        array2dSample =
            SquareArray2D.repeat
                length
                False
    in
    describe "elm-2d-array Benchmarks"
        [ describe "Normal 2D Array vs Array2D"
            [ Benchmark.compare "Creation"
                "Array"
                (\_ ->
                    Array.initialize
                        length
                        (\i -> Array.initialize length (\j -> (length * i) + j))
                )
                "SquareArray2D"
                (\_ ->
                    SquareArray2D.initialize
                        length
                        (\row column -> (length * row) + column)
                )
            , Benchmark.compare "Mapping"
                "Array"
                (\_ ->
                    arraySample
                        |> Array.map (\row -> Array.map not row)
                )
                "SquareArray2D"
                (\_ ->
                    array2dSample
                        |> SquareArray2D.map not
                )
            ]
        ]
