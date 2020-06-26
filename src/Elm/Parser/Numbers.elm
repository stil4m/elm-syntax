module Elm.Parser.Numbers exposing (forgivingNumber, integer)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Parser as Core


raw : (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = Just floatf
        }


{-| Strange case that a number is consumed and does not function in a `oneOf`
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser State a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw floatf intf hexf)
        |> Combine.fromCore


integer : (Int -> a) -> (Int -> a) -> Parser State a
integer intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = Nothing
        }
        |> Combine.fromCore
