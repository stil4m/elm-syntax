module List.Extra exposing (find, remove, unique)

{-| Helper for List functions. Taken from elm-community/list-extra.
-}


{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [ 0, 1, 1, 0, 1 ]
    --> [ 0, 1 ]

-}
unique : List a -> List a
unique list =
    uniqueHelp [] list []


uniqueHelp : List a -> List a -> List a -> List a
uniqueHelp existing remaining accumulator =
    case remaining of
        [] ->
            accumulator

        first :: rest ->
            if List.member first existing then
                uniqueHelp existing rest accumulator

            else
                uniqueHelp (first :: existing) rest (first :: accumulator)


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


{-| Remove the first occurrence of a value from a list.
-}
remove : a -> List a -> List a
remove x xs =
    removeHelp xs x xs []


removeHelp : List a -> a -> List a -> List a -> List a
removeHelp list x xs previousElements =
    case xs of
        [] ->
            list

        y :: ys ->
            if x == y then
                reverseAppend previousElements ys

            else
                removeHelp list x ys (y :: previousElements)


reverseAppend : List a -> List a -> List a
reverseAppend list1 list2 =
    List.foldl (::) list2 list1
