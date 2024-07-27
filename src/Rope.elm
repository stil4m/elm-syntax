module Rope exposing (Rope, RopeLikelyFilled(..), empty, flatFromList, one, prependTo, prependToLikelyFilled, toList)

{-| inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
-}


type alias Rope a =
    Maybe (RopeLikelyFilled a)


type RopeLikelyFilled a
    = Leaf a
    | Branch (List (RopeLikelyFilled a))


empty : Rope a
empty =
    Nothing


one : a -> RopeLikelyFilled a
one onlyElement =
    Leaf onlyElement


prependToLikelyFilled : Rope a -> RopeLikelyFilled a -> Rope a
prependToLikelyFilled right leftLikelyFilled =
    case right of
        Nothing ->
            Just leftLikelyFilled

        Just rightLikelyFilled ->
            Just (Branch [ leftLikelyFilled, rightLikelyFilled ])


prependTo : Rope a -> Rope a -> Rope a
prependTo right left =
    case left of
        Nothing ->
            right

        Just leftLikelyFilled ->
            case right of
                Nothing ->
                    left

                Just rightLikelyFilled ->
                    Just (Branch [ leftLikelyFilled, rightLikelyFilled ])


flatFromList : List (Rope a) -> Rope a
flatFromList ropes =
    case List.filterMap identity ropes of
        [] ->
            Nothing

        filledList ->
            Just (Branch filledList)


toList : Rope a -> List a
toList rope =
    case rope of
        Nothing ->
            []

        Just ropeLikelyFilled ->
            ropeLikelyFilledToListInto [] ropeLikelyFilled


ropeLikelyFilledToListInto : List a -> RopeLikelyFilled a -> List a
ropeLikelyFilledToListInto initialAcc ropeLikelyFilled =
    case ropeLikelyFilled of
        Leaf onlyElement ->
            onlyElement :: initialAcc

        Branch ropes ->
            List.foldr (\childRope childAcc -> ropeLikelyFilledToListInto childAcc childRope)
                initialAcc
                ropes
