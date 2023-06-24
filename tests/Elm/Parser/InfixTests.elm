module Elm.Parser.InfixTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Infix as Infix
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
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
                            { direction = Node empty Right
                            , precedence = Node empty 7
                            , operator = Node empty "</>"
                            , function = Node empty "slash"
                            }
                        )
        , test "left infix" <|
            \() ->
                parseFullStringState emptyState "infix left  8 (<?>) = questionMark" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = Node empty Left
                            , precedence = Node empty 8
                            , operator = Node empty "<?>"
                            , function = Node empty "questionMark"
                            }
                        )
        , test "non infix" <|
            \() ->
                parseFullStringState emptyState "infix non   4 (==) = eq" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = Node empty Non
                            , precedence = Node empty 4
                            , operator = Node empty "=="
                            , function = Node empty "eq"
                            }
                        )
        ]
