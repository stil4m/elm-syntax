module Elm.Parser.Ranges exposing (withCurrentPoint, withRange)

import Combine exposing (ParseLocation, Parser, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Location, Range)


asPointerLocation : ParseLocation -> Location
asPointerLocation { line, column } =
    { row = line, column = column }


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


withCurrentPoint : (Range -> Parser State a) -> Parser State a
withCurrentPoint p =
    withLocation
        (\start ->
            let
                k =
                    asPointerLocation start
            in
            p { start = k, end = k }
        )
