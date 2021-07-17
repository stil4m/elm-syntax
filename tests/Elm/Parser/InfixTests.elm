module Elm.Parser.InfixTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Infix as Infix exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "InfixTests"
        [ test "right infix" <|
            \() ->
                parseFullStringState emptyState "infix right 7 (</>) = slash" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = Node emptyRange Right
                            , precedence = Node emptyRange 7
                            , operator = Node emptyRange "</>"
                            , function = Node emptyRange "slash"
                            }
                        )
        , test "left infix" <|
            \() ->
                parseFullStringState emptyState "infix left  8 (<?>) = questionMark" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = Node emptyRange Left
                            , precedence = Node emptyRange 8
                            , operator = Node emptyRange "<?>"
                            , function = Node emptyRange "questionMark"
                            }
                        )
        , test "non infix" <|
            \() ->
                parseFullStringState emptyState "infix non   4 (==) = eq" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = Node emptyRange Non
                            , precedence = Node emptyRange 4
                            , operator = Node emptyRange "=="
                            , function = Node emptyRange "eq"
                            }
                        )
        ]
