module Elm.Parser.ExposeTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expose exposing (..)
import Elm.Syntax.Exposing exposing (..)
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
                    |> Expect.equal (Just ( emptyRange, InfixExpose "$>" ))
        , test "definitionExpose" <|
            \() ->
                parseFullStringWithNullState "Model" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just ( emptyRange, TypeOrAliasExpose "Model" ))
        , test "typeExpose" <|
            \() ->
                parseFullStringWithNullState "Msg(..)" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just ( emptyRange, TypeExpose (ExposedType "Msg" (Just emptyRange)) ))
        , test "exposingList" <|
            \() ->
                parseFullStringWithNullState "exposing (Model,Msg(..),Info(..),init,(::))" exposeDefinition
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ ( emptyRange, TypeOrAliasExpose "Model" )
                                , ( emptyRange, TypeExpose (ExposedType "Msg" (Just emptyRange)) )
                                , ( emptyRange, TypeExpose (ExposedType "Info" (Just emptyRange)) )
                                , ( emptyRange, FunctionExpose "init" )
                                , ( emptyRange, InfixExpose "::" )
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
                                [ ( emptyRange, FunctionExpose "foo" )
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
                                [ ( emptyRange, FunctionExpose "foo" )
                                ]
                            )
                        )
        , test "exposingList with spacing" <|
            \() ->
                parseFullStringWithNullState "exposing (Model, Msg, Info(..),init,(::) )" exposeDefinition
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ ( emptyRange, TypeOrAliasExpose "Model" )
                                , ( emptyRange, TypeOrAliasExpose "Msg" )
                                , ( emptyRange, TypeExpose (ExposedType "Info" (Just emptyRange)) )
                                , ( emptyRange, FunctionExpose "init" )
                                , ( emptyRange, InfixExpose "::" )
                                ]
                            )
                        )
        ]
