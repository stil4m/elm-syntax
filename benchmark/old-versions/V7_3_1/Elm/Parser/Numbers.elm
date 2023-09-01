module V7_3_1.Elm.Parser.Numbers exposing (forgivingNumber, number)

import Parser as Core
import V7_3_1.Combine as Combine exposing (Parser)
import V7_3_1.Elm.Parser.State exposing (State)


raw : (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = Just floatf
        }


{-| Strange case that a number is consumes and does not function in a `oneOf`
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw floatf intf hexf)
        |> Combine.fromCore


number : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
number floatf intf hexf =
    raw floatf intf hexf
        |> Combine.fromCore
