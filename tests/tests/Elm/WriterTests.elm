module Elm.WriterTests exposing (suite)

import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.ModuleName exposing (..)
import Elm.Syntax.Node exposing (Node(..))
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
                    Node emptyRange <|
                        NormalModule
                            { moduleName = Node emptyRange <| [ "A" ]
                            , exposingList = Node emptyRange <| All emptyRange
                            }
                , imports =
                    [ Node emptyRange
                        { moduleName = Node emptyRange <| [ "B" ]
                        , moduleAlias = Nothing
                        , exposingList = Nothing
                        }
                    , Node emptyRange
                        { moduleName = Node emptyRange <| [ "C" ]
                        , moduleAlias = Just (Node emptyRange [ "D" ])
                        , exposingList = Just (All emptyRange)
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
                    (Node emptyRange <| Application [ Node emptyRange <| FunctionOrValue [] "abc", Node emptyRange <| UnitExpr ])
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "abc ()"
            , test "write qualified expression" <|
                \() ->
                    (Node emptyRange <| FunctionOrValue [ "Foo", "Bar" ] "baz")
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "Foo.Bar.baz"
            ]
        , describe "TypeAnnotation"
            [ test "write simple type" <|
                \() ->
                    Elm.Syntax.TypeAnnotation.Typed (Node emptyRange <| ( [], "String" )) []
                        |> Node emptyRange
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "String"
            , test "write qualified type" <|
                \() ->
                    (Node emptyRange <|
                        Elm.Syntax.TypeAnnotation.Typed
                            (Node emptyRange <| ( [ "Json", "Decode" ], "Decoder" ))
                            [ Node emptyRange <| Elm.Syntax.TypeAnnotation.GenericType "a" ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "Json.Decode.Decoder a"
            , test "write type arguments that require parentheses" <|
                \() ->
                    (Node emptyRange <|
                        Elm.Syntax.TypeAnnotation.Typed (Node emptyRange ( [], "List" ))
                            [ Node emptyRange <|
                                Elm.Syntax.TypeAnnotation.Typed (Node emptyRange ( [], "Dict" ))
                                    [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Typed (Node emptyRange ( [], "String" )) []
                                    , Node emptyRange <| Elm.Syntax.TypeAnnotation.Typed (Node emptyRange ( [], "Int" )) []
                                    ]
                            ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "List (Dict String Int)"
            , test "write type arguments that are functions" <|
                \() ->
                    (Node emptyRange <|
                        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                            (Node emptyRange <|
                                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Node emptyRange <| Elm.Syntax.TypeAnnotation.GenericType "a")
                                    (Node emptyRange <| Elm.Syntax.TypeAnnotation.GenericType "b")
                            )
                            (Node emptyRange <| Elm.Syntax.TypeAnnotation.Typed (Node emptyRange ( [], "Int" )) [])
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "(a -> b) -> Int"
            ]
        , describe "Declaration"
            [ test "write type declaration" <|
                \() ->
                    (Node emptyRange <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node emptyRange "Sample")
                                []
                                [ Node emptyRange <| ValueConstructor (Node emptyRange "Foo") []
                                , Node emptyRange <| ValueConstructor (Node emptyRange "Bar") []
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
                                (CaseBlock (Node emptyRange <| FunctionOrValue [] "someCase")
                                    [ ( Node emptyRange <| IntPattern 1, Node emptyRange <| FunctionOrValue [] "doSomething" )
                                    , ( Node emptyRange <| IntPattern 2, Node emptyRange <| FunctionOrValue [] "doSomethingElse" )
                                    ]
                                )

                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node emptyRange <|
                                        FunctionImplementation
                                            (Node emptyRange <| "functionName")
                                            []
                                            (Node emptyRange body)
                                    )
                                )
                    in
                    Node emptyRange function
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
