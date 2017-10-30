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
                    |> Expect.equal (Just (InfixExpose "$>" emptyRange))
        , test "definitionExpose" <|
            \() ->
                parseFullStringWithNullState "Model" definitionExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just (TypeOrAliasExpose "Model" emptyRange))
        , test "typeExpose" <|
            \() ->
                parseFullStringWithNullState "Msg(Go,Back)" typeExpose
                    |> Maybe.map noRangeExpose
                    |> Expect.equal (Just (TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( "Go", emptyRange ), ( "Back", emptyRange ) ]) emptyRange)))
        , test "exposingList" <|
            \() ->
                parseFullStringWithNullState " exposing (Model,Msg(Go,Back),Info(..),init,(::))" (exposeDefinition exposable)
                    |> Maybe.map noRangeExposingList
                    |> Expect.equal
                        (Just
                            (Explicit
                                [ TypeOrAliasExpose "Model" emptyRange
                                , TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( "Go", emptyRange ), ( "Back", emptyRange ) ]) emptyRange)
                                , TypeExpose (ExposedType "Info" (Just <| All emptyRange) emptyRange)
                                , FunctionExpose "init" emptyRange
                                , InfixExpose "::" emptyRange
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
                                [ FunctionExpose "foo" emptyRange
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
                                [ FunctionExpose "foo" emptyRange
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
                                [ TypeOrAliasExpose "Model" emptyRange
                                , TypeExpose (ExposedType "Msg" (Just <| Explicit [ ( "Go", emptyRange ), ( "Back", emptyRange ) ]) emptyRange)
                                , TypeExpose (ExposedType "Info" (Just <| All emptyRange) emptyRange)
                                , FunctionExpose "init" emptyRange
                                , InfixExpose "::" emptyRange
                                ]
                            )
                        )
        ]
