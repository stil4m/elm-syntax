module Elm.Parser.NumbersTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Numbers as Parser
import Expect
import Test exposing (..)


all : Test
all =
    describe "NumbersTests"
        [ test "hex" <|
            \() ->
                parse "0x03FFFFFF" (Combine.fromCore (Parser.number (always Nothing) Just))
                    |> Expect.equal
                        (Just (Just 67108863))
        , test "hex - 2" <|
            \() ->
                parse "0xFF" (Combine.fromCore (Parser.number (always Nothing) Just))
                    |> Expect.equal
                        (Just (Just 255))
        ]
