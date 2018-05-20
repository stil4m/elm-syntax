module Elm.Parser.InfixTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Infix as Infix exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "InfixTests"
        [ test "right infix" <|
            \() ->
                parseFullStringState emptyState "infix right 7 (</>) = slash" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = ( emptyRange, Right )
                            , precedence = ( emptyRange, 7 )
                            , operator = ( emptyRange, "</>" )
                            , function = ( emptyRange, "slash" )
                            }
                        )
        , test "left infix" <|
            \() ->
                parseFullStringState emptyState "infix left  8 (<?>) = questionMark" Infix.infixDefinition
                    |> Maybe.map noRangeInfix
                    |> Expect.equal
                        (Just
                            { direction = ( emptyRange, Left )
                            , precedence = ( emptyRange, 8 )
                            , operator = ( emptyRange, "<?>" )
                            , function = ( emptyRange, "questionMark" )
                            }
                        )
        ]
