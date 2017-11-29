module Elm.Parser.ExposeTests exposing (..)

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
                parseFullStringWithNullState "Model" definitionExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just ( emptyRange, TypeOrAliasExpose "Model" ))
        , test "typeExpose" <|
            \() ->
                parseFullStringWithNullState "Msg(Go,Back)" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just ( emptyRange, TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( emptyRange, "Go" ), ( emptyRange, "Back" ) ])) ))
        , test "exposingList" <|
            \() ->
                parseFullStringWithNullState " exposing (Model,Msg(Go,Back),Info(..),init,(::))" (exposeDefinition exposable)
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ ( emptyRange, TypeOrAliasExpose "Model" )
                                , ( emptyRange, TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( emptyRange, "Go" ), ( emptyRange, "Back" ) ])) )
                                , ( emptyRange, TypeExpose (ExposedType "Info" (Just <| All emptyRange)) )
                                , ( emptyRange, FunctionExpose "init" )
                                , ( emptyRange, InfixExpose "::" )
                                ]
                            )
                        )
        , test "exposingListInner with comment" <|
            \() ->
                parseFullStringWithNullState "foo\n --bar\n " (exposingListInner exposable)
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
                parseFullStringWithNullState " exposing (foo\n --bar\n )" (exposeDefinition exposable)
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
                parseFullStringWithNullState " exposing (Model, Msg(Go,Back) , Info(..),init,(::) )" (exposeDefinition exposable)
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ ( emptyRange, TypeOrAliasExpose "Model" )
                                , ( emptyRange, TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( emptyRange, "Go" ), ( emptyRange, "Back" ) ])) )
                                , ( emptyRange, TypeExpose (ExposedType "Info" (Just <| All emptyRange)) )
                                , ( emptyRange, FunctionExpose "init" )
                                , ( emptyRange, InfixExpose "::" )
                                ]
                            )
                        )
        ]
