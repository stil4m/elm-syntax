module Elm.Syntax.ImportTests exposing (suite)

import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing as Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.ModuleName exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAnnotation
import Elm.Writer as Writer
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
                    symmetric (Import (Node emptyRange [ "A", "B" ]) (Just <| Node emptyRange "C") (Just <| Node emptyRange <| All emptyRange))
                        Import.encode
                        Import.decoder
            , test "import Html exposing (beginnerProgram, div, button, text)" <|
                \() ->
                    symmetric
                        (Import (Node emptyRange [ "Html" ])
                            Nothing
                            (Just <|
                                Node emptyRange <|
                                    Explicit
                                        [ Node emptyRange (FunctionExpose "beginnerProgram")
                                        , Node emptyRange (FunctionExpose "div")
                                        , Node emptyRange (FunctionExpose "button")
                                        , Node emptyRange (FunctionExpose "text")
                                        ]
                            )
                        )
                        Import.encode
                        Import.decoder
            ]
        ]
