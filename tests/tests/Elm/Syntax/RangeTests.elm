module Elm.Syntax.RangeTests exposing (suite)

import Elm.Syntax.Range as Range
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Elm.Syntax.Range"
        [ describe "compareLocations" compareLocationsTests
        ]


compareLocationsTests : List Test
compareLocationsTests =
    [ describe "EQ"
        [ test "when locations are equal" <|
            \() ->
                let
                    location : Range.Location
                    location =
                        { row = 1, column = 3 }
                in
                Range.compareLocations location location
                    |> Expect.equal EQ
        ]
    , describe "LT"
        [ test "when left row < right row" <|
            \() ->
                let
                    left : Range.Location
                    left =
                        { row = 1, column = 1 }

                    right : Range.Location
                    right =
                        { left | row = 3 }
                in
                Range.compareLocations left right
                    |> Expect.equal LT
        , test "when left row == right row and left column < right column" <|
            \() ->
                let
                    left : Range.Location
                    left =
                        { row = 1, column = 1 }

                    right : Range.Location
                    right =
                        { left | column = 3 }
                in
                Range.compareLocations left right
                    |> Expect.equal LT
        ]
    , describe "GT"
        [ test "when left row > right row" <|
            \() ->
                let
                    left : Range.Location
                    left =
                        { row = 3, column = 4 }

                    right : Range.Location
                    right =
                        { left | row = 2 }
                in
                Range.compareLocations left right
                    |> Expect.equal GT
        , test "when left row == right row and left column > right column" <|
            \() ->
                let
                    left : Range.Location
                    left =
                        { row = 2, column = 6 }

                    right : Range.Location
                    right =
                        { left | column = 3 }
                in
                Range.compareLocations left right
                    |> Expect.equal GT
        ]
    ]
