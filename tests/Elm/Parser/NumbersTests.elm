module Elm.Parser.NumbersTests exposing (all)

import Elm.Parser.TestUtil exposing (..)
import Expect
import ParserFast
import Test exposing (..)


all : Test
all =
    describe "NumbersTests"
        [ describe "integerDecimalOrHexadecimalMapWithRange"
            [ test "hex" <|
                \() ->
                    parseToResult "0x03FFFFFF" (ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ _ -> -1) (\_ n -> n))
                        |> Expect.equal
                            (Ok 67108863)
            , test "hex - 2" <|
                \() ->
                    parseToResult "0xFF" (ParserFast.integerDecimalOrHexadecimalMapWithRange (\_ _ -> -1) (\_ n -> n))
                        |> Expect.equal
                            (Ok 255)
            ]
        , describe "floatOrIntegerDecimalOrHexadecimalMapWithRange"
            [ test "hex" <|
                \() ->
                    parseToResult "0x2A"
                        (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
                            (\_ _ -> -1)
                            (\_ _ -> -1)
                            (\_ n -> n)
                        )
                        |> Expect.equal
                            (Ok 42)

            -- TODO handling overflow like elm-format / the elm compiler
            -- would technically be a breaking change and maybe somewhat difficult to implement
            -- If there's a decision in issues like
            --   - https://github.com/stil4m/elm-syntax/issues/108
            --   - https://github.com/stil4m/elm-syntax/issues/255
            -- this should be considered.
            -- , test "overflow int" <|
            --     \() ->
            --         parseToResult "100000000000000000000000"
            --             (ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
            --                 (\_ _ -> -1)
            --                 (\_ n -> n)
            --                 (\_ _ -> -1)
            --             )
            --             |> Expect.equal
            --                 (Ok 200376420520689660)
            ]
        ]
