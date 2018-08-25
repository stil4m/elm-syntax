module Combine.Num exposing (float, int)

import Combine exposing (Parser, string, succeed)
import Parser as Core


unwrap : res -> (String -> Maybe res) -> String -> res
unwrap default f s =
    case f s of
        Just res ->
            res

        Nothing ->
            default


toInt : String -> Int
toInt =
    unwrap 0 String.toInt


toFloat : String -> Float
toFloat =
    unwrap 0 String.toFloat


sign : Parser s Int
sign =
    Combine.optional 1
        (Combine.choice
            [ succeed 1 |> Combine.ignore (string "+")
            , succeed -1 |> Combine.ignore (string "-")
            ]
        )


int : Parser s Int
int =
    Combine.fromCore Core.int


float : Parser s Float
float =
    Combine.fromCore Core.float
