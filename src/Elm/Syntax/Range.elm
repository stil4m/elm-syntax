module Elm.Syntax.Range exposing
    ( Range, Location
    , emptyRange, combine
    , compare, compareLocations
    , encode, decoder
    )

{-|


## Types

@docs Range, Location


## Functions

@docs emptyRange, combine


## Comparison

See also [Basics.compare](https://package.elm-lang.org/packages/elm/core/latest/Basics#compare).

@docs compare, compareLocations


## Serialization

@docs encode, decoder

-}

import Json.Decode as JD exposing (Decoder)
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


{-| Encode a range
-}
encode : Range -> Value
encode { start, end } =
    JE.list JE.int
        [ start.row
        , start.column
        , end.row
        , end.column
        ]


{-| Decode a range
-}
decoder : Decoder Range
decoder =
    JD.list JD.int
        |> JD.andThen fromList


fromList : List Int -> Decoder Range
fromList input =
    case input of
        [ a, b, c, d ] ->
            JD.succeed
                { start = { row = a, column = b }
                , end = { row = c, column = d }
                }

        _ ->
            JD.fail "Invalid input list"


{-| Compute the largest area of a list of ranges.
-}
combine : List Range -> Range
combine ranges =
    case ranges of
        [] ->
            emptyRange

        head :: tail ->
            combineHelp tail head.start head.end


combineHelp : List Range -> Location -> Location -> Range
combineHelp ranges previousStart previousEnd =
    case ranges of
        [] ->
            { start = previousStart, end = previousEnd }

        { start, end } :: rest ->
            let
                newStart : Location
                newStart =
                    case compareLocations start previousStart of
                        LT ->
                            start

                        _ ->
                            previousStart

                newEnd : Location
                newEnd =
                    case compareLocations end previousEnd of
                        GT ->
                            end

                        _ ->
                            previousEnd
            in
            combineHelp rest newStart newEnd


{-| Compare the position of two Ranges.
-}
compare : Range -> Range -> Order
compare left right =
    case compareLocations left.start right.start of
        EQ ->
            compareLocations left.end right.end

        order ->
            order


{-| Compare two Locations.
-}
compareLocations : Location -> Location -> Order
compareLocations left right =
    if left.row < right.row then
        LT

    else if left.row > right.row then
        GT

    else
        Basics.compare left.column right.column
