module Elm.Syntax.Infix exposing (Infix, InfixDirection(Left, Right))

{-| Infix Syntax

#Types

@docs Infix, InfixDirection

-}


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
