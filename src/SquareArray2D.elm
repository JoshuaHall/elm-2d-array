module SquareArray2D exposing
    ( fromRows, fromRowMajor, filledWith
    , numRows, numColumns, numElements, get
    , set, update
    , map, indexedMap
    , toFlatArrayRowMajor
    , generator
    , SquareArray2D
    )

{-| Provides an ergonomic and fast way to use 2 dimensional arrays in Elm.


# Square 2D Arrays

@docs Array2D


# Creation

@docs fromRows, fromRowMajor, filledWith


# Query

@docs numRows, numColumns, numElements, get


# Manipulation

@docs set, update


# Transform

@docs map, indexedMap


# Lists

@docs toFlatArrayRowMajor


# Randomness

@docs generator

-}

import Array exposing (Array)
import ArrayHelpers
import Random exposing (Generator)
import Random.Array as RandomArray


{-| Type representing a 2 dimensional square array. Internally is a single Array with a side length.
-}
type SquareArray2D a
    = InternalSquareArray2D
        { array : Array a
        , sideLength : Int
        }


fromRows : Array (Array a) -> Maybe (SquareArray2D a)
fromRows rowsArrays =
    let
        rowLen : Int
        rowLen =
            case Array.get 0 rowsArrays of
                Just firstRow ->
                    Array.length firstRow

                Nothing ->
                    0
    in
    if Array.length rowsArrays == rowLen && ArrayHelpers.arrayAll (\arr -> Array.length arr == rowLen) rowsArrays then
        InternalSquareArray2D
            { array = ArrayHelpers.arrayConcat rowsArrays
            , sideLength = rowLen
            }
            |> Just

    else
        Nothing


fromRowMajor : Int -> Array a -> Maybe (SquareArray2D a)
fromRowMajor sideLength array =
    if (sideLength ^ 2) /= Array.length array then
        Nothing

    else
        InternalSquareArray2D
            { array = array
            , sideLength = sideLength
            }
            |> Just


filledWith : a -> Int -> SquareArray2D a
filledWith value sideLength =
    InternalSquareArray2D
        { array = Array.repeat (sideLength ^ 2) value
        , sideLength = sideLength
        }


numElements : SquareArray2D a -> Int
numElements (InternalSquareArray2D { sideLength }) =
    sideLength ^ 2


numRows : SquareArray2D a -> Int
numRows (InternalSquareArray2D array) =
    array.sideLength


numColumns : SquareArray2D a -> Int
numColumns (InternalSquareArray2D array) =
    array.sideLength


get : Int -> Int -> SquareArray2D a -> Maybe a
get row column (InternalSquareArray2D array) =
    if row >= array.sideLength || column >= array.sideLength then
        Nothing

    else
        Array.get
            (indexFromRowAndColumn (InternalSquareArray2D array) row column)
            array.array


set : Int -> Int -> a -> SquareArray2D a -> SquareArray2D a
set row column value (InternalSquareArray2D array) =
    InternalSquareArray2D
        { array
            | array =
                Array.set
                    (indexFromRowAndColumn (InternalSquareArray2D array) row column)
                    value
                    array.array
        }


update : Int -> Int -> (a -> a) -> SquareArray2D a -> SquareArray2D a
update row column updater (InternalSquareArray2D array) =
    let
        maybeElement : Maybe a
        maybeElement =
            get row column (InternalSquareArray2D array)
    in
    case maybeElement of
        Just element ->
            set row column (updater element) (InternalSquareArray2D array)

        Nothing ->
            InternalSquareArray2D array


map : (a -> b) -> SquareArray2D a -> SquareArray2D b
map fn (InternalSquareArray2D array) =
    InternalSquareArray2D
        { array = Array.map fn array.array
        , sideLength = array.sideLength
        }


indexedMap : (Int -> Int -> a -> b) -> SquareArray2D a -> SquareArray2D b
indexedMap fn (InternalSquareArray2D array) =
    InternalSquareArray2D
        { array =
            Array.indexedMap
                (indexedMapHelper array.sideLength fn)
                array.array
        , sideLength = array.sideLength
        }


{-| Internal use only.
-}
indexedMapHelper : Int -> (Int -> Int -> a -> b) -> Int -> a -> b
indexedMapHelper sideLength fn index elem =
    let
        ( row, column ) =
            indexToRowAndColumn sideLength index
    in
    fn row column elem


toFlatArrayRowMajor : SquareArray2D a -> Array a
toFlatArrayRowMajor (InternalSquareArray2D array) =
    array.array


{-| Generates a grid where each square is randomly filled in.
-}
generator : Generator a -> Int -> Generator (SquareArray2D a)
generator valueGenerator sideLength =
    Random.map2
        (\array sideLength_ ->
            InternalSquareArray2D
                { array = array
                , sideLength = sideLength_
                }
        )
        (RandomArray.array sideLength valueGenerator)
        (Random.constant sideLength)


{-| Internal use only.
-}
indexFromRowAndColumn : SquareArray2D a -> Int -> Int -> Int
indexFromRowAndColumn (InternalSquareArray2D array) row column =
    row * array.sideLength + column


{-| Internal use only.
-}
indexToRowAndColumn : Int -> Int -> ( Int, Int )
indexToRowAndColumn sideLength index =
    ( index // sideLength, modBy sideLength index )
