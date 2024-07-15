module Elm.Parser.Numbers exposing (forgivingNumber, number)

import Combine exposing (Parser)
import Parser as Core


raw : Maybe (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { int = Just intf
        , hex = Just hexf
        , octal = Nothing
        , binary = Nothing
        , float = floatf
        }


{-| Core.number bug: consumes leading '.' or 'e'
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw (Just floatf) intf hexf)


number : (Int -> a) -> (Int -> a) -> Parser state a
number intf hexf =
    raw Nothing intf hexf
        |> Combine.fromCore
