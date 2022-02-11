module Elm.WriterTests exposing (suite)

import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
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
                        , moduleAlias = Just (Node emptyRange "D")
                        , exposingList = Just (Node emptyRange <| All emptyRange)
                        }
                    ]
                , declarations = []
                , comments = []
                }
                    |> Writer.writeFile
                    |> Writer.write
                    |> Expect.equal
                        ("module A exposing (..)\n"
                            ++ "import B  \n"
                            ++ "import C as D exposing (..)\n"
                        )
        , describe "Expression"
            [ test "write simple expression" <|
                \() ->
                    (Node emptyRange <|
                        Application
                            (Node emptyRange <| FunctionOrValue [] "abc")
                            [ Node emptyRange <| TupleExpression [] ]
                    )
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
        , describe "Pattern"
            [ test "write string pattern" <|
                \() ->
                    StringPattern "test"
                        |> Node emptyRange
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\""
            , test "write string pattern containing \"" <|
                \() ->
                    StringPattern "test\""
                        |> Node emptyRange
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\\\"\""
            ]
        , describe "TypeAnnotation"
            [ test "write simple type" <|
                \() ->
                    Elm.Syntax.TypeAnnotation.Type (Node emptyRange <| ( [], "String" )) []
                        |> Node emptyRange
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "String"
            , test "write qualified type" <|
                \() ->
                    (Node emptyRange <|
                        Elm.Syntax.TypeAnnotation.Type
                            (Node emptyRange <| ( [ "Json", "Decode" ], "Decoder" ))
                            [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Var "a" ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "Json.Decode.Decoder a"
            , test "write type arguments that require parentheses" <|
                \() ->
                    (Node emptyRange <|
                        Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "List" ))
                            [ Node emptyRange <|
                                Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "Dict" ))
                                    [ Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "String" )) []
                                    , Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "Int" )) []
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
                                    (Node emptyRange <| Elm.Syntax.TypeAnnotation.Var "a")
                                    (Node emptyRange <| Elm.Syntax.TypeAnnotation.Var "b")
                            )
                            (Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "Int" )) [])
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "(a -> b) -> Int"
            ]
        , describe "Declaration"
            [ test "write type declaration > simple constructors" <|
                \() ->
                    (Node emptyRange <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node emptyRange "Sample")
                                []
                                (Node emptyRange <| ValueConstructor (Node emptyRange "Foo") [])
                                [ Node emptyRange <| ValueConstructor (Node emptyRange "Bar") [] ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo  | Bar "
                            )
            , test "write type declaration > constructors with arguments" <|
                \() ->
                    let
                        listT =
                            Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "List" ))
                                [ Node emptyRange <|
                                    Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "String" )) []
                                ]

                        stringT =
                            Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "String" )) []
                    in
                    (Node emptyRange <|
                        CustomTypeDeclaration
                            (Elm.Syntax.Type.Type
                                Nothing
                                (Node emptyRange "Sample")
                                []
                                (Node emptyRange <| ValueConstructor (Node emptyRange "Foo") [ Node emptyRange listT, Node emptyRange stringT ])
                                [ Node emptyRange <| ValueConstructor (Node emptyRange "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (List String) String | Bar "
                            )
            , test "write type declaration > constructors with functions as arguments" <|
                \() ->
                    let
                        funcT =
                            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "String" )) [])
                                (Node emptyRange <| Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "Int" )) [])

                        stringT =
                            Elm.Syntax.TypeAnnotation.Type (Node emptyRange ( [], "String" )) []
                    in
                    (Node emptyRange <|
                        CustomTypeDeclaration
                            (Elm.Syntax.Type.Type
                                Nothing
                                (Node emptyRange "Sample")
                                []
                                (Node emptyRange <|
                                    ValueConstructor (Node emptyRange "Foo")
                                        [ Node emptyRange funcT
                                        , Node emptyRange stringT
                                        ]
                                )
                                [ Node emptyRange <| ValueConstructor (Node emptyRange "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (String -> Int) String | Bar "
                            )
            , test "write function with case expression using the right indentations" <|
                \() ->
                    let
                        body =
                            CaseExpression
                                (CaseBlock (Node emptyRange <| FunctionOrValue [] "someCase")
                                    ( Node emptyRange <| IntPattern 1, Node emptyRange <| FunctionOrValue [] "doSomething" )
                                    [ ( Node emptyRange <| IntPattern 2, Node emptyRange <| FunctionOrValue [] "doSomethingElse" ) ]
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
                                ++ "    \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    "
                            )
            , test "regression test for incorrect indentation in case expression" <|
                \() ->
                    let
                        body =
                            LambdaExpression
                                { firstArg = Node emptyRange (VarPattern_ "myArgument")
                                , restOfArgs = []
                                , expression =
                                    Node emptyRange <|
                                        CaseExpression
                                            (CaseBlock (Node emptyRange <| FunctionOrValue [] "someCase")
                                                ( Node emptyRange <| IntPattern 1, Node emptyRange <| FunctionOrValue [] "doSomething" )
                                                [ ( Node emptyRange <| IntPattern 2, Node emptyRange <| FunctionOrValue [] "doSomethingElse" ) ]
                                            )
                                }

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
                                ++ "    \\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    "
                            )
            , test "regression test for incorrect parenthesis placement in case expression" <|
                \() ->
                    let
                        body =
                            TupleExpression
                                [ Node emptyRange <|
                                    LambdaExpression
                                        { firstArg = Node emptyRange (VarPattern_ "myArgument")
                                        , restOfArgs = []
                                        , expression =
                                            Node emptyRange <|
                                                CaseExpression
                                                    (CaseBlock (Node emptyRange <| FunctionOrValue [] "someCase")
                                                        ( Node emptyRange <| IntPattern 1, Node emptyRange <| FunctionOrValue [] "doSomething" )
                                                        [ ( Node emptyRange <| IntPattern 2, Node emptyRange <| FunctionOrValue [] "doSomethingElse" ) ]
                                                    )
                                        }
                                ]

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
                                ++ "    (\\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        doSomething\n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    )"
                            )
            , test "nested case expressions" <|
                \() ->
                    let
                        body nested =
                            Node emptyRange <|
                                CaseExpression
                                    (CaseBlock (Node emptyRange <| FunctionOrValue [] "someCase")
                                        ( Node emptyRange <| IntPattern 1, nested )
                                        [ ( Node emptyRange <| IntPattern 2, Node emptyRange <| FunctionOrValue [] "doSomethingElse" ) ]
                                    )

                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node emptyRange <|
                                        FunctionImplementation
                                            (Node emptyRange <| "functionName")
                                            []
                                            (Node emptyRange
                                                (TupleExpression
                                                    [ Node emptyRange <|
                                                        LambdaExpression
                                                            { firstArg = Node emptyRange (VarPattern_ "myArgument")
                                                            , restOfArgs = []
                                                            , expression =
                                                                body (body (Node emptyRange (TupleExpression [])))
                                                            }
                                                    ]
                                                )
                                            )
                                    )
                                )
                    in
                    Node emptyRange function
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("\n\nfunctionName  =\n"
                                ++ "    (\\myArgument -> \n"
                                ++ "    case someCase of\n"
                                ++ "      1 ->\n"
                                ++ "        \n"
                                ++ "        case someCase of\n"
                                ++ "          1 ->\n"
                                ++ "            ()\n"
                                ++ "          2 ->\n"
                                ++ "            doSomethingElse\n"
                                ++ "        \n"
                                ++ "      2 ->\n"
                                ++ "        doSomethingElse\n"
                                ++ "    )"
                            )
            ]
        , describe "Tuple"
            [ test "write tuple" <|
                \() ->
                    (Node emptyRange <|
                        TupleExpression
                            [ Node emptyRange <| Integer 1
                            , Node emptyRange <| Integer 2
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "(1, 2)"
            ]
        ]
