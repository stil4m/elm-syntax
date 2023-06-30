module Elm.Parser.GlslTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expression as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node
import Expect
import Test exposing (..)


all : Test
all =
    describe "GlslTests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "[glsl| precision mediump float; |]" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Expect.equal (Just (Node.empty <| GLSL " precision mediump float; "))
        ]
