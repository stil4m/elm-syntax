module Elm.WriterTests exposing (..)

import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Writer as Writer
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Writer"
        [ test "write file exposing all" <|
            \() ->
                { moduleDefinition =
                    NormalModule
                        { moduleName = [ "A" ]
                        , exposingList = All emptyRange
                        }
                , imports =
                    [ { moduleName = [ "B" ]
                      , moduleAlias = Nothing
                      , exposingList = Nothing
                      , range = emptyRange
                      }
                    , { moduleName = [ "C" ]
                      , moduleAlias = Just [ "D" ]
                      , exposingList = Just (All emptyRange)
                      , range = emptyRange
                      }
                    ]
                , declarations = []
                , comments = []
                }
                    |> Writer.writeFile
                    |> Writer.write
                    |> Expect.equal
                        ("""module A exposing (..)
import B  """
                            ++ "\n"
                            ++ """import C as D exposing (..)
"""
                        )
        , test "write simple expression" <|
            \() ->
                ( Elm.Syntax.Range.emptyRange, Application [ ( Elm.Syntax.Range.emptyRange, FunctionOrValue "abc" ), ( Elm.Syntax.Range.emptyRange, UnitExpr ) ] )
                    |> Writer.writeExpression
                    |> Writer.write
                    |> Expect.equal "abc ()"
        ]
