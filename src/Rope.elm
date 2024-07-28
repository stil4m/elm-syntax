module Rope exposing (Rope, RopeFilled(..), empty, filledPrependTo, one, prependTo, toList)

{-| inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
-}


type alias Rope a =
    Maybe (RopeFilled a)


type RopeFilled a
    = Leaf a
    | Branch2 { left : RopeFilled a, right : RopeFilled a }


empty : Rope a
empty =
    Nothing


one : a -> RopeFilled a
one onlyElement =
    Leaf onlyElement


filledPrependTo : Rope a -> RopeFilled a -> Rope a
filledPrependTo right leftLikelyFilled =
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


ropeLikelyFilledToListInto : List a -> RopeFilled a -> List a
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
