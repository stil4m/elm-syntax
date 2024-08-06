module Elm.Parser.Numbers exposing (floatOrIntOrHex, intOrHex)

import ParserFast


floatOrIntOrHex : (Float -> a) -> (Int -> a) -> (Int -> a) -> ParserFast.Parser a
floatOrIntOrHex floatf intf hexf =
    ParserFast.number
        { binary = Nothing
        , float = Just floatf
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }


intOrHex : (Int -> a) -> (Int -> a) -> ParserFast.Parser a
intOrHex intf hexf =
    ParserFast.number
        { binary = Nothing
        , float = Nothing
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }
