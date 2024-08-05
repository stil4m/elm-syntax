module Elm.Parser.Numbers exposing (floatOrIntOrHex, intOrHex)

import CustomParser


floatOrIntOrHex : (Float -> a) -> (Int -> a) -> (Int -> a) -> CustomParser.Parser a
floatOrIntOrHex floatf intf hexf =
    CustomParser.number
        { binary = Nothing
        , float = Just floatf
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }


intOrHex : (Int -> a) -> (Int -> a) -> CustomParser.Parser a
intOrHex intf hexf =
    CustomParser.number
        { binary = Nothing
        , float = Nothing
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }
