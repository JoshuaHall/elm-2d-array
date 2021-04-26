module ArrayHelpers exposing (arrayAll, arrayConcat, indexFromRowAndColumn, indexToRowAndColumn, list2dToArray2d, squareArrayIndexToRowAndColumn)

import Array exposing (Array)


list2dToArray2d : List (List a) -> Array (Array a)
list2dToArray2d =
    List.map Array.fromList >> Array.fromList


arrayAll : (a -> Bool) -> Array a -> Bool
arrayAll predicate =
    Array.foldl
        (\elem acc -> predicate elem && acc)
        True


arrayConcat : Array (Array a) -> Array a
arrayConcat =
    Array.foldr
        Array.append
        Array.empty


indexFromRowAndColumn : Int -> Int -> Int -> Int
indexFromRowAndColumn sideLength row column =
    row * sideLength + column


indexToRowAndColumn : Int -> Int -> ( Int, Int )
indexToRowAndColumn columns index =
    ( index // columns, modBy columns index )


squareArrayIndexToRowAndColumn : Int -> Int -> ( Int, Int )
squareArrayIndexToRowAndColumn sideLength index =
    ( index // sideLength, modBy sideLength index )
