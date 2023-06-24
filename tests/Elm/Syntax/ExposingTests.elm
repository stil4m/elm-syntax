module Elm.Syntax.ExposingTests exposing (suite)

import Elm.Syntax.Exposing as Exposing exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
import Expect
import Json.Decode exposing (Decoder, Value)
import Test exposing (..)


symmetric : a -> (a -> Value) -> Decoder a -> Expect.Expectation
symmetric v enc dec =
    Expect.equal (Json.Decode.decodeValue dec (enc v)) (Ok v)


suite : Test
suite =
    describe "Elm.Syntax.Exposing"
        [ describe "serialization"
            [ test "exposing (beginnerProgram, div, button, text)" <|
                \() ->
                    symmetric
                        (Explicit
                            [ Node empty (FunctionExpose "beginnerProgram")
                            , Node empty (FunctionExpose "div")
                            , Node empty (FunctionExpose "button")
                            , Node empty (FunctionExpose "text")
                            ]
                        )
                        Exposing.encode
                        Exposing.decoder
            ]
        ]
