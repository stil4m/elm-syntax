module Elm.Parser.Numbers exposing (forgivingNumber, number)

import Parser


raw : Maybe (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser.Parser a
raw floatf intf hexf =
    Parser.number
        { binary = Nothing
        , float = floatf
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }


{-| Parser.number bug: consumes leading '.' or 'e'
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser.Parser a
forgivingNumber floatf intf hexf =
    Parser.backtrackable (raw (Just floatf) intf hexf)


number : (Int -> a) -> (Int -> a) -> Parser.Parser a
number intf hexf =
    raw Nothing intf hexf
