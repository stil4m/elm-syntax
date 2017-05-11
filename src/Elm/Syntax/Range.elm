module Elm.Syntax.Range exposing (Range, Location, emptyRange, combine, encode, decode)

{-| Source Code Ranges


# Types

@docs Range, Location


# Functions

@docs emptyRange, combine


# Json

@docs encode, decode

-}

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra exposing (fromResult)


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


{-| Encode a range
-}
encode : Range -> Value
encode { start, end } =
    JE.list
        [ JE.int start.row
        , JE.int start.column
        , JE.int end.row
        , JE.int end.column
        ]


{-| Decode a range
-}
decode : Decoder Range
decode =
    JD.list JD.int
        |> JD.andThen
            (fromList >> fromResult)


fromList : List Int -> Result String Range
fromList input =
    case input of
        [ a, b, c, d ] ->
            Ok
                { start = { row = a, column = b }
                , end = { row = c, column = d }
                }

        _ ->
            Err "Invalid input list"


{-| Compute the smallest area of a list of ranges.
-}
combine : List Range -> Range
combine ranges =
    let
        starts =
            List.map .start ranges |> sortLocations

        ends =
            List.map .end ranges |> sortLocations |> List.reverse
    in
        Maybe.map2 Range (List.head starts) (List.head ends)
            |> Maybe.withDefault emptyRange


{-| Could be faster via single fold
-}
sortLocations : List Location -> List Location
sortLocations =
    List.sortWith compareLocations


compareLocations : Location -> Location -> Order
compareLocations left right =
    if left.row < right.row then
        LT
    else if right.row < left.row then
        GT
    else
        compare left.column right.column
