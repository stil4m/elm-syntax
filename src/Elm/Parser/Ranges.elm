module Elm.Parser.Ranges exposing (ranged, rangedWithCustomStart, withRange, withRangeCustomStart)

import Combine exposing (ParseLocation, Parser, andMap, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Ranged exposing (Ranged)


asPointerLocation : ParseLocation -> Location
asPointerLocation { line, column } =
    { row = line, column = column }


withRangeCustomStart : Range -> Parser State (Range -> a) -> Parser State a
withRangeCustomStart { start } p =
    p
        |> andMap
            (withLocation
                (\end ->
                    succeed <|
                        { start = start
                        , end = asPointerLocation end
                        }
                )
            )


withRange : Parser State (Range -> a) -> Parser State a
withRange p =
    withLocation
        (\start ->
            p
                |> Combine.andMap
                    (withLocation
                        (\end ->
                            succeed <|
                                { start = asPointerLocation start
                                , end = asPointerLocation end
                                }
                        )
                    )
        )


ranged : Parser State a -> Parser State (Ranged a)
ranged p =
    withLocation
        (\start ->
            succeed (\v r -> ( r, v ))
                |> Combine.andMap p
                |> Combine.andMap
                    (withLocation
                        (\end ->
                            succeed <|
                                { start = asPointerLocation start
                                , end = asPointerLocation end
                                }
                        )
                    )
        )


rangedWithCustomStart : Range -> Parser State a -> Parser State (Ranged a)
rangedWithCustomStart { start } p =
    succeed (\a b -> ( b, a ))
        |> Combine.andMap p
        |> Combine.andMap
            (withLocation
                (\end ->
                    succeed <|
                        { start = start
                        , end = asPointerLocation end
                        }
                )
            )
