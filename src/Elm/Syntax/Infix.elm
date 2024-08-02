module Elm.Syntax.Infix exposing (Infix, InfixDirection(..))

{-|


## Types

@docs Infix, InfixDirection

-}

import Elm.Syntax.Node exposing (Node)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non
