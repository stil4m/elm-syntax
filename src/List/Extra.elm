module List.Extra exposing (find)

{-| Helper for List functions. find and unique taken from elm-community/list-extra.

@docs find

-}


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                find predicate xs
