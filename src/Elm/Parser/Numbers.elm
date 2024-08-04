module Elm.Parser.Numbers exposing (forgivingNumber, number)

import CustomParser


raw : Maybe (Float -> a) -> (Int -> a) -> (Int -> a) -> CustomParser.Parser a
raw floatf intf hexf =
    CustomParser.number
        { binary = Nothing
        , float = floatf
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }


{-| CustomParser.number bug: consumes leading '.' or 'e'
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> CustomParser.Parser a
forgivingNumber floatf intf hexf =
    CustomParser.backtrackable (raw (Just floatf) intf hexf)


number : (Int -> a) -> (Int -> a) -> CustomParser.Parser a
number intf hexf =
    raw Nothing intf hexf
