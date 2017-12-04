module Elm.Syntax.Ranged exposing (Ranged)

{-| Ranged Syntax Elements


# Types

@docs Ranged

-}

import Elm.Syntax.Range exposing (Range)


{-| Tuple wrapping a source element with a Range
-}
type alias Ranged a =
    ( Range, a )
