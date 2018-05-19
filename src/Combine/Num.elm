module Combine.Num exposing (float, int)

import Combine exposing (Parser, string, succeed)


unwrap : res -> (String -> Maybe res) -> String -> res
unwrap default f s =
    case f s of
        Just res ->
            res

        Nothing ->
            -- Debug.todo "impossible state in Combine.Num.unwrap"
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
    Combine.succeed (*)
        |> Combine.andMap sign
        |> Combine.andMap (Combine.map toInt (Combine.regex "(0|[1-9][0-9]*)"))
        |> Combine.setError "expected an integer"


float : Parser s Float
float =
    Combine.succeed ((*) << Basics.toFloat)
        |> Combine.andMap sign
        |> Combine.andMap (Combine.map toFloat (Combine.regex "(0|[1-9][0-9]*)(\\.[0-9]+)"))
        |> Combine.setError "expected a float"
