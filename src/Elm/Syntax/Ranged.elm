module Elm.Syntax.Ranged exposing (Ranged, value)

{-| Ranged Syntax Elements


# Types

@docs Ranged, value

-}

import Elm.Syntax.Range exposing (Range)


{-| Tuple wrapping a source element with a Range
-}
type alias Ranged a =
    ( Range, a )


{-| Get the value of the Ranged thing
-}
value : Ranged a -> a
value =
    Tuple.second
