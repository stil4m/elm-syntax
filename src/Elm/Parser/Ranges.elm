module Elm.Parser.Ranges exposing (withRange)

import Combine exposing (Parser, succeed, withLocation)
import Elm.Syntax.Range exposing (Range)


withRange : Parser s (Range -> a) -> Parser s a
withRange p =
    withLocation
        (\start ->
            p
                |> Combine.keep
                    (withLocation
                        (\end ->
                            succeed
                                { start = start
                                , end = end
                                }
                        )
                    )
        )
