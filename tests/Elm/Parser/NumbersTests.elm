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
                parse "0x03FFFFFF" (ParserFast.intOrHexMapWithRange (\_ _ -> Nothing) (\n _ -> Just n))
                    |> Expect.equal
                        (Just (Just 67108863))
        , test "hex - 2" <|
            \() ->
                parse "0xFF" (ParserFast.intOrHexMapWithRange (\_ _ -> Nothing) (\n _ -> Just n))
                    |> Expect.equal
                        (Just (Just 255))
        ]
