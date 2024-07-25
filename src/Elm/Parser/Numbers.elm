module Elm.Parser.Numbers exposing (forgivingNumber, integer)

import Parser as Core


raw : Maybe (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
raw floatf intf hexf =
    Core.number
        { binary = Nothing
        , float = floatf
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }


{-| Core.number bug: consumes leading '.' or 'e'
-}
forgivingNumber : (Float -> a) -> (Int -> a) -> (Int -> a) -> Core.Parser a
forgivingNumber floatf intf hexf =
    Core.backtrackable (raw (Just floatf) intf hexf)


integer : (Int -> a) -> (Int -> a) -> Core.Parser a
integer intf hexf =
    Core.number
        { binary = Nothing
        , float = Nothing
        , hex = Just hexf
        , int = Just intf
        , octal = Nothing
        }
