module Elm.Syntax.Range exposing (Range, Location)

{-| Source Code Ranges


# Types

@docs Range, Location

-}


{-| Source location
-}
type alias Location =
    { row : Int
    , column : Int
    }


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }
