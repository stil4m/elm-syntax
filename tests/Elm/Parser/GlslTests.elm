module Elm.Parser.GlslTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Expect
import Elm.Parser.Declarations as Parser exposing (..)
import Test exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)


all : Test
all =
    describe "GlslTests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "[glsl| precision mediump float; |]" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Expect.equal (Just (emptyRanged <| GLSLExpression " precision mediump float; "))
        ]
