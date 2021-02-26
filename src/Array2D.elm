module Array2D exposing
    ( Array2D
    , fromRows, fromColumns, fromRowMajor, initialize, repeat
    , numRows, numColumns, numElements, get
    , set, update
    , map, indexedMap
    , toFlatArrayRowMajor
    , generator
    )

{-| Provides an ergonomic and fast way to use 2 dimensional arrays in Elm.


# 2D Arrays

@docs Array2D


# Creation

@docs fromRows, fromColumns, fromRowMajor, initialize, repeat


# Query

@docs numRows, numColumns, numElements, get


# Manipulation

@docs set, update


# Transform

@docs map, indexedMap


# Arrays

@docs toFlatArrayRowMajor


# Randomness

@docs generator

-}

import Array exposing (Array)
import ArrayHelpers exposing (indexToRowAndColumn)
import Maybe.Extra as MaybeExtra
import Random exposing (Generator)
import Random.Array as RandomArray


{-| Type representing a 2 dimensional array.
-}
type Array2D a
    = InternalArray2D
        { array : Array a
        , rows : Int
        , columns : Int
        }


{-| Attempts to create a `Array2D` from an `Array` of `Array`s representing rows.
Returns `Nothing` when the rows' lengths don't match.
-}
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


{-| Attempts to create a `Array2D` from an `Array` of `Array`s representing columns.
Returns `Nothing` when the columns' lengths don't match.
-}
fromColumns : Array (Array a) -> Maybe (Array2D a)
fromColumns columnsArrays =
    let
        columnLen : Int
        columnLen =
            case Array.get 0 columnsArrays of
                Just firstColumn ->
                    Array.length firstColumn

                Nothing ->
                    0
    in
    if ArrayHelpers.arrayAll (\arr -> Array.length arr == columnLen) columnsArrays then
        let
            rows : Int
            rows =
                columnLen

            columns : Int
            columns =
                Array.length columnsArrays

            indicesRowMajor : List ( Int, Int )
            indicesRowMajor =
                List.range 0 (rows - 1)
                    |> List.concatMap
                        (\row ->
                            List.range 0 (columns - 1)
                                |> List.map (\column -> ( row, column ))
                        )

            listOfMaybes : List (Maybe a)
            listOfMaybes =
                indicesRowMajor
                    |> List.map
                        (\( row, column ) ->
                            columnsArrays
                                |> Array.get column
                                |> Maybe.withDefault Array.empty
                                |> Array.get row
                        )
        in
        listOfMaybes
            |> MaybeExtra.combine
            |> Maybe.map Array.fromList
            |> Maybe.andThen
                (\array ->
                    InternalArray2D
                        { array = array
                        , rows = rows
                        , columns = columns
                        }
                        |> Just
                )

    else
        Nothing


{-| Attempts to create a `Array2D` from an `Array` of elements and the rows and columns amounts.
Orders the elements in [row major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order).
The array has to be the same length as the product of the provided rows and columns.
-}
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


{-| Creates an `Array2D` with the provided rows and columns.
Each element is created using the provided function which gives the index (row and column).
-}
initialize : Int -> Int -> (Int -> Int -> a) -> Array2D a
initialize rows columns fn =
    InternalArray2D
        { array =
            Array.initialize
                (rows * columns)
                (\index ->
                    let
                        ( row, column ) =
                            indexToRowAndColumn rows columns index
                    in
                    fn row column
                )
        , rows = rows
        , columns = columns
        }


{-| Creates a `Array2D` with the provided rows and columns and fills it with the provided element.
-}
repeat : Int -> Int -> a -> Array2D a
repeat rows columns value =
    initialize
        rows
        columns
        (\_ _ -> value)


{-| Gets the total number of elements the `Array2D` holds.
-}
numElements : Array2D a -> Int
numElements (InternalArray2D { rows, columns }) =
    rows * columns


{-| Gets the number of rows the `Array2D` holds.
-}
numRows : Array2D a -> Int
numRows (InternalArray2D array) =
    array.rows


{-| Gets the number of columns the `SquareArray2D` holds.
-}
numColumns : Array2D a -> Int
numColumns (InternalArray2D array) =
    array.columns


{-| Gets an element from the `Array2D` at the provided row and column.
Returns `Nothing` when the index is out of bounds.
-}
get : Int -> Int -> Array2D a -> Maybe a
get row column (InternalArray2D array) =
    if row >= array.rows || column >= array.columns then
        Nothing

    else
        Array.get
            (indexFromRowAndColumn (InternalArray2D array) row column)
            array.array


{-| Sets an element in the `Array2D` at the provided row and column.
Returns the `Array2D` unchanged if the index is out of bounds.
-}
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


{-| Updates an element in the `Array2D` with a function at the provided row and column.
Returns the `Array2D` unchanged if the index is out of bounds.
-}
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


{-| Applies a function to every element in the `SquareArray2D`.
-}
map : (a -> b) -> Array2D a -> Array2D b
map fn (InternalArray2D array) =
    InternalArray2D
        { array = Array.map fn array.array
        , rows = array.rows
        , columns = array.columns
        }


{-| Applies a function to every element in the `SquareArray2D`.
The index (row and column) are provided to the mapping function.
-}
indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap fn (InternalArray2D array) =
    let
        indexedMapHelper : (Int -> Int -> a -> b) -> Int -> a -> b
        indexedMapHelper mapFn index elem =
            let
                ( row, column ) =
                    indexToRowAndColumn array.rows array.columns index
            in
            mapFn row column elem
    in
    InternalArray2D
        { array =
            Array.indexedMap
                (indexedMapHelper fn)
                array.array
        , rows = array.rows
        , columns = array.columns
        }


{-| Flattens the `Array2D` in [row major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order).
-}
toFlatArrayRowMajor : Array2D a -> Array a
toFlatArrayRowMajor (InternalArray2D array) =
    array.array


indexFromRowAndColumn : Array2D a -> Int -> Int -> Int
indexFromRowAndColumn (InternalArray2D array) row column =
    row * array.columns + column


{-| Generates a `SquareArray2D` where each value is randomly generated.
-}
generator : Generator a -> Int -> Int -> Generator (Array2D a)
generator valueGenerator rows columns =
    Random.map
        (\array ->
            InternalArray2D
                { array = array
                , rows = rows
                , columns = columns
                }
        )
        (RandomArray.array (rows * columns) valueGenerator)
