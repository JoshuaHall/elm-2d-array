module Array2D exposing
    ( Array2D
    , fromRows, fromRowMajor, filledWith
    , numRows, numColumns, numElements
    , get, set, update
    , toFlatArrayRowMajor
    )

{-| Provides an ergonomic and fast way to use 2 dimensional arrays in Elm.


# Definition

@docs Array2D


# Array2D Creation

@docs fromRows, fromRowMajor, filledWith


# Information

@docs numRows, numColumns, numElements


# Array2D Access

@docs get, set, update


# Conversion

@docs toFlatArrayRowMajor

-}

import Array exposing (Array)
import ArrayHelpers


{-| Type representing a 2 dimensional array. Internally is a single Array with a row and column count.
-}
type Array2D a
    = InternalArray2D
        { array : Array a
        , rows : Int
        , columns : Int
        }


fromRows : Array (Array a) -> Maybe (Array2D a)
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
    if ArrayHelpers.arrayAll (\arr -> Array.length arr == rowLen) rowsArrays then
        InternalArray2D
            { array = ArrayHelpers.arrayConcat rowsArrays
            , rows = Array.length rowsArrays
            , columns = rowLen
            }
            |> Just

    else
        Nothing


fromRowMajor : Int -> Int -> Array a -> Maybe (Array2D a)
fromRowMajor rows columns array =
    if (rows * columns) /= Array.length array then
        Nothing

    else
        InternalArray2D
            { array = array
            , rows = rows
            , columns = columns
            }
            |> Just


filledWith : a -> Int -> Int -> Array2D a
filledWith value rows columns =
    InternalArray2D
        { array = Array.repeat (rows * columns) value
        , rows = rows
        , columns = columns
        }


numElements : Array2D a -> Int
numElements (InternalArray2D { rows, columns }) =
    rows * columns


numRows : Array2D a -> Int
numRows (InternalArray2D array) =
    array.rows


numColumns : Array2D a -> Int
numColumns (InternalArray2D array) =
    array.columns


get : Int -> Int -> Array2D a -> Maybe a
get row column (InternalArray2D array) =
    if row >= array.rows || column >= array.columns then
        Nothing

    else
        Array.get
            (indexFromRowAndColumn (InternalArray2D array) row column)
            array.array


set : Int -> Int -> a -> Array2D a -> Array2D a
set row column value (InternalArray2D array) =
    InternalArray2D
        { array
            | array =
                Array.set
                    (indexFromRowAndColumn (InternalArray2D array) row column)
                    value
                    array.array
        }


update : Int -> Int -> (a -> a) -> Array2D a -> Array2D a
update row column updater (InternalArray2D array) =
    let
        maybeElement : Maybe a
        maybeElement =
            get row column (InternalArray2D array)
    in
    case maybeElement of
        Just element ->
            set row column (updater element) (InternalArray2D array)

        Nothing ->
            InternalArray2D array


toFlatArrayRowMajor : Array2D a -> Array a
toFlatArrayRowMajor (InternalArray2D array) =
    array.array


indexFromRowAndColumn : Array2D a -> Int -> Int -> Int
indexFromRowAndColumn (InternalArray2D array) row column =
    row * array.columns + column
