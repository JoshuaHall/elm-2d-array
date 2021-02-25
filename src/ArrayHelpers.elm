module ArrayHelpers exposing (arrayAll, arrayConcat)

import Array exposing (Array)


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
