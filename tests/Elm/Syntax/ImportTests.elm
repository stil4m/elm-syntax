module Elm.Syntax.ImportTests exposing (suite)

import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (empty)
import Expect
import Json.Decode exposing (Decoder, Value)
import Test exposing (..)


symmetric : a -> (a -> Value) -> Decoder a -> Expect.Expectation
symmetric v enc dec =
    Expect.equal (Json.Decode.decodeValue dec (enc v)) (Ok v)


suite : Test
suite =
    describe "Elm.Syntax.Import"
        [ describe "serialization"
            [ test "case 1" <|
                \() ->
                    symmetric (Import (Node.empty [ "A", "B" ]) (Just <| Node.empty [ "C" ]) (Just <| Node.empty <| All empty))
                        Import.encode
                        Import.decoder
            , test "import Html exposing (beginnerProgram, div, button, text)" <|
                \() ->
                    symmetric
                        (Import (Node.empty [ "Html" ])
                            Nothing
                            (Just <|
                                Node.empty <|
                                    Explicit
                                        [ Node.empty (FunctionExpose "beginnerProgram")
                                        , Node.empty (FunctionExpose "div")
                                        , Node.empty (FunctionExpose "button")
                                        , Node.empty (FunctionExpose "text")
                                        ]
                            )
                        )
                        Import.encode
                        Import.decoder
            ]
        ]
