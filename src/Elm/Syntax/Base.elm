module Elm.Syntax.Base exposing (ModuleName, VariablePointer)

{-| Base Syntax


# Types

@docs ModuleName, VariablePointer

-}

import Elm.Syntax.Range exposing (Range)


{-| Base representation for a module name
-}
type alias ModuleName =
    List String


{-| A simple variable
-}
type alias VariablePointer =
    { value : String
    , range : Range
    }
