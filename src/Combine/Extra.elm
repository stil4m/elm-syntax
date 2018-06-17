module Combine.Extra exposing (continueWith, ignore)

import Combine exposing (Parser)


ignore : Parser s x -> Parser s a -> Parser s a
ignore dropped target =
    target
        |> Combine.map always
        |> Combine.andMap dropped


continueWith : Parser s a -> Parser s x -> Parser s a
continueWith target dropped =
    dropped
        |> Combine.map (\b a -> always a b)
        |> Combine.andMap target
