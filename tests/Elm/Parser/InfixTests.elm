module Elm.Parser.InfixTests exposing (all)

import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "InfixTests"
        [ test "right infix" <|
            \() ->
                "infix right 7 (</>) = slash"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            (Declaration.InfixDeclaration
                                { direction = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } } Right
                                , precedence = Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 7
                                , operator = Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "</>"
                                , function = Node { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } } "slash"
                                }
                            )
                        )
        , test "left infix" <|
            \() ->
                "infix left  8 (<?>) = questionMark"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 35 } }
                            (Declaration.InfixDeclaration
                                { direction = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } Left
                                , precedence = Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 8
                                , operator = Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "<?>"
                                , function = Node { start = { row = 1, column = 23 }, end = { row = 1, column = 35 } } "questionMark"
                                }
                            )
                        )
        , test "non infix" <|
            \() ->
                "infix non   4 (==) = eq"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            (Declaration.InfixDeclaration
                                { direction = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } Non
                                , precedence = Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 4
                                , operator = Node { start = { row = 1, column = 15 }, end = { row = 1, column = 19 } } "=="
                                , function = Node { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } } "eq"
                                }
                            )
                        )
        ]


expectAst : Node Declaration -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst declaration
