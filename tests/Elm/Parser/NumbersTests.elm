module Elm.Parser.NumbersTests exposing (all)

import Elm.Parser.TestUtil exposing (..)
import Expect
import ParserFast
import Test exposing (..)


all : Test
all =
    describe "NumbersTests"
        [ test "hex" <|
            \() ->
                parse "0x03FFFFFF" (ParserFast.intOrHex (always Nothing) Just)
                    |> Expect.equal
                        (Just (Just 67108863))
        , test "hex - 2" <|
            \() ->
                parse "0xFF" (ParserFast.intOrHex (always Nothing) Just)
                    |> Expect.equal
                        (Just (Just 255))
        ]
