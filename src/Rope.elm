module Rope exposing (Rope, RopeLikelyFilled(..), empty, likelyFilledPrependTo, one, prependTo, toList)

{-| inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
-}


type alias Rope a =
    Maybe (RopeLikelyFilled a)


type RopeLikelyFilled a
    = Leaf a
    | Branch2 { left : RopeLikelyFilled a, right : RopeLikelyFilled a }


empty : Rope a
empty =
    Nothing


one : a -> RopeLikelyFilled a
one onlyElement =
    Leaf onlyElement


likelyFilledPrependTo : Rope a -> RopeLikelyFilled a -> Rope a
likelyFilledPrependTo right leftLikelyFilled =
    case right of
        Nothing ->
            Just leftLikelyFilled

        Just rightLikelyFilled ->
            Just (Branch2 { left = leftLikelyFilled, right = rightLikelyFilled })


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
                    Just (Branch2 { left = leftLikelyFilled, right = rightLikelyFilled })


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

        Branch2 ropes ->
            ropeLikelyFilledToListInto
                (ropeLikelyFilledToListInto
                    initialAcc
                    ropes.right
                )
                ropes.left
