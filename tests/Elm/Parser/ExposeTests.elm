module Elm.Parser.ExposeTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expose exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


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
                    |> expectAst (All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } })
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
                    |> expectAst
                        (Explicit
                            [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
                                (FunctionExpose "foo")
                            ]
                        )
        ]


expectAst : Exposing -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source exposeDefinition of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parseFullStringWithNullState source exposeDefinition of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
