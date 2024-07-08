module Elm.Parser.ExposeTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ExposeTests"
        [ test "Exposing all" <|
            \() ->
                "exposing (..)"
                    |> expectAst (All { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } })
        , test "Exposing all with spacing and comment" <|
            \() ->
                """exposing (
  .. -- foo
  )"""
                    |> expectAstWithComments
                        { ast = All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                        , comments = [ Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
                        }
        , test "should fail to parse multi-line exposing all when closing parens is at the end of a line" <|
            \() ->
                """exposing (
  ..
)"""
                    |> expectInvalid
        , test "should fail to parse empty with just 1 `.`" <|
            \() ->
                "exposing ( . )"
                    |> expectInvalid
        , test "should fail to parse empty with just 3 `...`" <|
            \() ->
                "exposing ( ... )"
                    |> expectInvalid
        , test "should fail to parse empty with 2 spaced `.`" <|
            \() ->
                "exposing (. .)"
                    |> expectInvalid
        , test "should fail to parse empty exposing list" <|
            \() ->
                "exposing ()"
                    |> expectInvalid
        , test "Explicit exposing list" <|
            \() ->
                "exposing (Model,Msg(..),Info(..),init,(::))"
                    |> expectAst
                        (Explicit
                            [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } (TypeOrAliasExpose "Model")
                            , Node { start = { row = 1, column = 17 }, end = { row = 1, column = 24 } }
                                (TypeExpose
                                    { name = "Msg"
                                    , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                    }
                                )
                            , Node { start = { row = 1, column = 25 }, end = { row = 1, column = 33 } }
                                (TypeExpose
                                    { name = "Info"
                                    , open = Just { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } }
                                    }
                                )
                            , Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (FunctionExpose "init")
                            , Node { start = { row = 1, column = 39 }, end = { row = 1, column = 43 } } (InfixExpose "::")
                            ]
                        )
        , test "exposingList with spacing on one line" <|
            \() ->
                "exposing (Model, Msg, Info   (..)   ,init,(::) )"
                    |> expectAst
                        (Explicit
                            [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } (TypeOrAliasExpose "Model")
                            , Node { start = { row = 1, column = 18 }, end = { row = 1, column = 21 } } (TypeOrAliasExpose "Msg")
                            , Node { start = { row = 1, column = 23 }, end = { row = 1, column = 34 } }
                                (TypeExpose
                                    { name = "Info"
                                    , open = Just { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } }
                                    }
                                )
                            , Node { start = { row = 1, column = 38 }, end = { row = 1, column = 42 } } (FunctionExpose "init")
                            , Node { start = { row = 1, column = 43 }, end = { row = 1, column = 47 } } (InfixExpose "::")
                            ]
                        )
        , test "Explicit exposing list with spaces and newlines" <|
            \() ->
                """exposing
    ( A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )"""
                    |> expectAst
                        (Explicit
                            [ Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (TypeOrAliasExpose "A")
                            , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                (TypeExpose
                                    { name = "B"
                                    , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                    }
                                )
                            , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                (TypeExpose
                                    { name = "Info"
                                    , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                    }
                                )
                            , Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (FunctionExpose "init")
                            , Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (InfixExpose "::")
                            ]
                        )
        , test "Comments inside the exposing clause" <|
            \() ->
                "exposing (foo\n --bar\n )"
                    |> expectAstWithComments
                        { ast =
                            Explicit
                                [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
                                    (FunctionExpose "foo")
                                ]
                        , comments = [ Node { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } } "--bar" ]
                        }
        ]


expectAst : Exposing -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst exposeDefinition


expectAstWithComments : { ast : Exposing, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    CombineTestUtil.expectAstWithComments exposeDefinition


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid exposeDefinition
