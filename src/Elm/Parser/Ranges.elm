module Elm.Parser.Ranges exposing (withCurrentPoint, withRange)

import Combine exposing (Parser, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Range)


withRange : Parser State (Range -> a) -> Parser State a
withRange p =
    withLocation
        (\start ->
            p
                |> Combine.andMap
                    (withLocation
                        (\end ->
                            succeed <|
                                { start = start
                                , end = end
                                }
                        )
                    )
        )


withCurrentPoint : (Range -> Parser State a) -> Parser State a
withCurrentPoint p =
    withLocation
        (\start ->
            p { start = start, end = start }
        )
