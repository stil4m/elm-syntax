module Elm.Parser.NumbersTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Numbers as Parser
import Expect
import Test exposing (..)


all : Test
all =
    describe "NumbersTests"
        [ test "hex" <|
            \() ->
                parseFullStringWithNullState "0x03FFFFFF" (Parser.number (always Nothing) (always Nothing) Just)
                    |> Expect.equal
                        (Just (Just 67108863))
        , test "hex - 2" <|
            \() ->
                parseFullStringWithNullState "0xFF" (Parser.number (always Nothing) (always Nothing) Just)
                    |> Expect.equal
                        (Just (Just 255))
        ]
