module Elm.Parser.Numbers exposing (number)

import Combine exposing (Parser)
import Combine.Num as Num
import Elm.Parser.State exposing (State)


number : (Float -> a) -> (Int -> a) -> Parser State a
number floatf intf =
    Combine.choice
        [ Combine.map floatf Num.float
        , Combine.map intf Num.int
        ]
