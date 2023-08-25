module Elm.Parser.ExposeTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expose exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ExposeTests"
        [ test "infixExpose" <|
            \() ->
                parseFullStringWithNullState "($>)" infixExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just (Node.empty <| InfixExpose "$>"))
        , test "definitionExpose" <|
            \() ->
                parseFullStringWithNullState "Model" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just (Node.empty <| TypeOrAliasExpose "Model"))
        , test "typeExpose" <|
            \() ->
                parseFullStringWithNullState "Msg(..)" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just (Node.empty <| TypeExpose (ExposedType "Msg" (Just empty))))
        , test "Explicit exposing list" <|
            \() ->
                parseFullStringWithNullState "exposing (Model,Msg(..),Info(..),init,(::))" exposeDefinition
                    |> Expect.equal
                        (Just
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
                        )
        , test "Explicit exposing list with spaces and newlines" <|
            \() ->
                parseFullStringWithNullState """exposing
    ( A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )""" exposeDefinition
                    |> Expect.equal
                        (Just
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
                        )
        , test "exposingListInner with comment" <|
            \() ->
                parseFullStringWithNullState "foo\n --bar\n " exposingListInner
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ Node empty <| FunctionExpose "foo"
                                ]
                            )
                        )
        , test "exposingList with comment 2" <|
            \() ->
                parseFullStringWithNullState "exposing (foo\n --bar\n )" exposeDefinition
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ Node empty <| FunctionExpose "foo"
                                ]
                            )
                        )
        , test "exposingList with spacing" <|
            \() ->
                parseFullStringWithNullState "exposing (Model, Msg, Info   (..)   ,init,(::) )" exposeDefinition
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ Node empty <| TypeOrAliasExpose "Model"
                                , Node empty <| TypeOrAliasExpose "Msg"
                                , Node empty <| TypeExpose (ExposedType "Info" (Just empty))
                                , Node empty <| FunctionExpose "init"
                                , Node empty <| InfixExpose "::"
                                ]
                            )
                        )
        , describe "ranges"
            [ test "exposed item should not include trailing whitespace in range" <|
                let
                    input =
                        """exposing
    ( Link
    , init
    )"""
                in
                \() ->
                    parseFullStringWithNullState input exposeDefinition
                        |> Expect.equal
                            (Just
                                (Explicit
                                    [ Node { start = { row = 2, column = 7 }, end = { row = 2, column = 11 } }
                                        (TypeOrAliasExpose "Link")
                                    , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 11 } }
                                        (FunctionExpose "init")
                                    ]
                                )
                            )
            ]
        ]
