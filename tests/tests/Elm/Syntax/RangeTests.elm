module Elm.Syntax.RangeTests exposing (suite)

import Elm.Syntax.Range as Range exposing (Range)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Elm.Syntax.Range"
        [ describe "combine" combineTests
        , describe "compareLocations" compareLocationsTests
        , describe "comparePositions" comparePositionsTests
        ]


combineTests : List Test
combineTests =
    [ test "given an empty list is an empty range" <|
        \() ->
            Range.combine []
                |> Expect.equal Range.emptyRange
    , test "given a singleton list is the same range" <|
        \() ->
            let
                range : Range
                range =
                    { start = { row = 1, column = 1 }
                    , end = { row = 1, column = 9 }
                    }
            in
            Range.combine [ range ]
                |> Expect.equal range
    , test "given a list of ranges, where one covers all the others, gives that range" <|
        \() ->
            let
                outerRange : Range
                outerRange =
                    { start = { row = 1, column = 1 }
                    , end = { row = 5, column = 100 }
                    }

                ranges : List Range
                ranges =
                    [ outerRange
                    , { start = { row = 2, column = 1 }
                      , end = { row = 2, column = 9 }
                      }
                    , { start = { row = 3, column = 5 }
                      , end = { row = 4, column = 15 }
                      }
                    ]
            in
            Range.combine ranges
                |> Expect.equal outerRange
    , test "given a list of distinct ranges where none overlap, gives a range over all" <|
        \() ->
            let
                outerRange : Range
                outerRange =
                    { start = { row = 2, column = 1 }
                    , end = { row = 10, column = 69 }
                    }

                ranges : List Range
                ranges =
                    [ { start = outerRange.start
                      , end = { row = 2, column = 21 }
                      }
                    , { start = { row = 4, column = 6 }
                      , end = { row = 4, column = 100 }
                      }
                    , { start = { row = 9, column = 5 }
                      , end = outerRange.end
                      }
                    ]
            in
            Range.combine ranges
                |> Expect.equal outerRange
    , test "given overlapping ranges, gives a range over both" <|
        \() ->
            let
                outerRange : Range
                outerRange =
                    { start = { row = 1, column = 23 }
                    , end = { row = 32, column = 5 }
                    }

                ranges : List Range
                ranges =
                    [ { start = { row = 15, column = 1 }
                      , end = outerRange.end
                      }
                    , { start = outerRange.start
                      , end = { row = 16, column = 27 }
                      }
                    ]
            in
            Range.combine ranges
                |> Expect.equal outerRange
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
