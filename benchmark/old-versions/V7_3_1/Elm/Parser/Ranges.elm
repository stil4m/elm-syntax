module V7_3_1.Elm.Parser.Ranges exposing (withCurrentPoint, withRange)

import V7_3_1.Combine as Combine exposing (ParseLocation, Parser, succeed, withLocation)
import V7_3_1.Elm.Parser.State exposing (State)
import V7_3_1.Elm.Syntax.Range exposing (Location, Range)


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
