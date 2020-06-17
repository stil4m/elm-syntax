module Elm.Syntax.RangeTests exposing (suite)

import Elm.Syntax.Range as Range exposing (Range)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Elm.Syntax.Range"
        [ describe "compareLocations" compareLocationsTests
        , describe "comparePositions" comparePositionsTests
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


comparePositionsTests : List Test
comparePositionsTests =
    [ describe "EQ"
        [ test "when ranges are equal" <|
            \() ->
                let
                    range : Range
                    range =
                        { start = { row = 1, column = 2 }
                        , end = { row = 3, column = 4 }
                        }
                in
                Range.comparePositions range range
                    |> Expect.equal EQ
        ]
    , describe "LT"
        [ test "when left start < right start" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 1, column = 1 }
                        , end = { row = 1, column = 2 }
                        }

                    right : Range
                    right =
                        { left | end = { row = 2, column = 2 } }
                in
                Range.comparePositions left right
                    |> Expect.equal LT
        , test "when left start == right start and left end < right end" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 2, column = 1 }
                        , end = { row = 2, column = 3 }
                        }

                    right : Range
                    right =
                        { left | end = { row = 2, column = 9 } }
                in
                Range.comparePositions left right
                    |> Expect.equal LT
        ]
    , describe "GT"
        [ test "when left start > right start" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 5, column = 6 }
                        , end = { row = 5, column = 7 }
                        }

                    right : Range
                    right =
                        { left | start = { row = 5, column = 2 } }
                in
                Range.comparePositions left right
                    |> Expect.equal GT
        , test "when left start == right start and left end > right end" <|
            \() ->
                let
                    left : Range
                    left =
                        { start = { row = 5, column = 2 }
                        , end = { row = 5, column = 9 }
                        }

                    right : Range
                    right =
                        { left | end = { row = 5, column = 4 } }
                in
                Range.comparePositions left right
                    |> Expect.equal GT
        ]
    ]
