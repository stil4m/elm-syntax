module Elm.Parser.NumbersTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Numbers as Parser
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "NumbersTests"
        [ test "hex" <|
            \() ->
                parseFullStringWithNullState "0x03FFFFFF" (Parser.number (always Nothing) (always Nothing) Just)
                    |> Expect.equal
                        (Just (Just 67108863))
        ]
