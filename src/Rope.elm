module Rope exposing (Rope(..), empty, flatFromList, fromList, one, toList)

{-| inspired by [miniBill/elm-rope](https://dark.elm.dmy.fr/packages/miniBill/elm-rope/latest/)
-}


type Rope a
    = Leaf (List a)
    | Branch (List (Rope a))


fromList : List a -> Rope a
fromList list =
    Leaf list


empty : Rope a
empty =
    Leaf []


one : a -> Rope a
one onlyElement =
    Leaf (List.singleton onlyElement)


flatFromList : List (Rope a) -> Rope a
flatFromList ropes =
    Branch ropes


toList : Rope a -> List a
toList rope =
    toListInto [] rope


toListInto : List a -> Rope a -> List a
toListInto initialAcc rope =
    case rope of
        Leaf list ->
            List.foldr (::) initialAcc list

        Branch ropes ->
            List.foldr (\childRope childAcc -> toListInto childAcc childRope)
                initialAcc
                ropes
