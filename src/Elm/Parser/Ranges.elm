module Elm.Parser.Ranges exposing (ranged, rangedWithCustomStart, withRange, withRangeCustomStart)

import Combine exposing ((<$>), (<*>), ParseLocation, Parser, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Ranged exposing (Ranged)


asPointerLocation : ParseLocation -> Location
asPointerLocation { line, column } =
    { row = line, column = column }


withRangeCustomStart : Range -> Parser State (Range -> a) -> Parser State a
withRangeCustomStart { start } p =
    p
        <*> withLocation
                (\end ->
                    succeed <|
                        { start = start
                        , end = asPointerLocation end
                        }
                )


withRange : Parser State (Range -> a) -> Parser State a
withRange p =
    withLocation
        (\start ->
            p
                <*> withLocation
                        (\end ->
                            succeed <|
                                { start = asPointerLocation start
                                , end = asPointerLocation end
                                }
                        )
        )


ranged : Parser State a -> Parser State (Ranged a)
ranged p =
    withLocation
        (\start ->
            (flip (,) <$> p)
                <*> withLocation
                        (\end ->
                            succeed <|
                                { start = asPointerLocation start
                                , end = asPointerLocation end
                                }
                        )
        )


rangedWithCustomStart : Range -> Parser State a -> Parser State (Ranged a)
rangedWithCustomStart { start } p =
    (flip (,) <$> p)
        <*> withLocation
                (\end ->
                    succeed <|
                        { start = start
                        , end = asPointerLocation end
                        }
                )
