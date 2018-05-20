module Elm.Parser.GlslTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "GlslTests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "[glsl| precision mediump float; |]" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Expect.equal (Just (emptyRanged <| GLSLExpression " precision mediump float; "))
        ]
