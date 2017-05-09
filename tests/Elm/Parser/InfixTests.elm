module Elm.Parser.InfixTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Expect
import Test exposing (..)
import Elm.Parser.Infix as Infix exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Infix exposing (..)


all : Test
all =
    describe "InfixTests"
        [ test "right infix" <|
            \() ->
                parseFullStringState emptyState "infixr 3 <<" Infix.infixDefinition
                    |> Expect.equal (Just { direction = Right, precedence = 3, operator = "<<" })
        , test "left infix" <|
            \() ->
                parseFullStringState emptyState "infixl 5 >>" Infix.infixDefinition
                    |> Expect.equal (Just { direction = Left, precedence = 5, operator = ">>" })
        , test "infix neutral" <|
            \() ->
                parseFullStringState emptyState "infix 2 >>" Infix.infixDefinition
                    |> Expect.equal (Just { direction = Left, precedence = 2, operator = ">>" })
        ]
