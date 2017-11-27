module Elm.Parser.Ranges exposing (withRange, withRangeCustomStart, withRangeTuple)

import Combine exposing ((<*>), ParseLocation, Parser, succeed, withLocation, withState)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Location, Range)


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


withRangeTuple : Parser State (Range -> a) -> Parser State ( Range, a )
withRangeTuple p =
    withRange (succeed (\pv r -> ( r, pv r )) <*> p)
