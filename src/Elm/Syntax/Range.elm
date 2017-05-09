module Elm.Syntax.Range exposing (Range, Location, emptyRange, encode)

{-| Source Code Ranges


# Types

@docs Range, Location


# Functions

@docs emptyRange


# Json

@docs encode

-}

import Json.Encode as JE exposing (Value)


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


{-| Construct an empty range
-}
emptyRange : Range
emptyRange =
    { start = { row = 0, column = 0 }
    , end = { row = 0, column = 0 }
    }


{-| Encode range
-}
encode : Range -> Value
encode { start, end } =
    JE.list
        [ JE.int start.row
        , JE.int start.column
        , JE.int end.row
        , JE.int end.column
        ]
