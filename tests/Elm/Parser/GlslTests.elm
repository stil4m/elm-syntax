module Elm.Parser.GlslTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expression exposing (expression)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "GlslTests"
        [ test "case block" <|
            \() ->
                "[glsl| precision mediump float; |]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            (GLSLExpression " precision mediump float; ")
                        )
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parse source expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected
