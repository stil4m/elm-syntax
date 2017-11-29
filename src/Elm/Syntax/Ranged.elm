module Elm.Syntax.Ranged exposing (Ranged)

import Elm.Syntax.Range exposing (Range)


type alias Ranged a =
    ( Range, a )
