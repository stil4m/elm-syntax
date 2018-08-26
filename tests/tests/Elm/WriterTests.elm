module Elm.WriterTests exposing (suite)

import Elm.Syntax.Base exposing (..)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAnnotation
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
        , describe "Expression"
            [ test "write simple expression" <|
                \() ->
                    ( Elm.Syntax.Range.emptyRange, Application [ ( Elm.Syntax.Range.emptyRange, FunctionOrValue "abc" ), ( Elm.Syntax.Range.emptyRange, UnitExpr ) ] )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "abc ()"
            , test "write qualified expression" <|
                \() ->
                    ( Elm.Syntax.Range.emptyRange, QualifiedExpr [ "Foo", "Bar" ] "baz" )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "Foo.Bar.baz"
            ]
        , describe "TypeAnnotation"
            [ test "write simple type" <|
                \() ->
                    ( emptyRange, Elm.Syntax.TypeAnnotation.Typed [] "String" [] )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "String"
            , test "write qualified type" <|
                \() ->
                    ( emptyRange
                    , Elm.Syntax.TypeAnnotation.Typed
                        [ "Json", "Decode" ]
                        "Decoder"
                        [ ( emptyRange, Elm.Syntax.TypeAnnotation.GenericType "a" ) ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "Json.Decode.Decoder a"
            , test "write type arguments that require parentheses" <|
                \() ->
                    ( emptyRange
                    , Elm.Syntax.TypeAnnotation.Typed []
                        "List"
                        [ ( emptyRange
                          , Elm.Syntax.TypeAnnotation.Typed []
                                "Dict"
                                [ ( emptyRange, Elm.Syntax.TypeAnnotation.Typed [] "String" [] )
                                , ( emptyRange, Elm.Syntax.TypeAnnotation.Typed [] "Int" [] )
                                ]
                          )
                        ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "List (Dict String Int)"
            , test "write type arguments that are functions" <|
                \() ->
                    ( emptyRange
                    , Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                        ( emptyRange
                        , Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                            ( emptyRange, Elm.Syntax.TypeAnnotation.GenericType "a" )
                            ( emptyRange, Elm.Syntax.TypeAnnotation.GenericType "b" )
                        )
                        ( emptyRange, Elm.Syntax.TypeAnnotation.Typed [] "Int" [] )
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "(a -> b) -> Int"
            ]
        , describe "Declaration"
            [ test "write type declaration" <|
                \() ->
                    ( emptyRange
                    , TypeDecl
                        (Type "Sample"
                            []
                            [ ValueConstructor "Foo" [] emptyRange
                            , ValueConstructor "Bar" [] emptyRange
                            ]
                        )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "=Foo |Bar "
                            )
            , test "write function with case expression using the right indentations" <|
                \() ->
                    let
                        body =
                            CaseExpression
                                (CaseBlock ( emptyRange, FunctionOrValue "someCase" )
                                    [ ( ( emptyRange, IntPattern 1 ), ( emptyRange, FunctionOrValue "doSomething" ) )
                                    , ( ( emptyRange, IntPattern 2 ), ( emptyRange, FunctionOrValue "doSomethingElse" ) )
                                    ]
                                )

                        function =
                            FuncDecl
                                (Function Nothing
                                    Nothing
                                    (FunctionDeclaration
                                        (VariablePointer "functionName" emptyRange)
                                        []
                                        ( emptyRange, body )
                                    )
                                )
                    in
                    ( emptyRange, function )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse"
                            )
            ]
        ]
