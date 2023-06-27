module Elm.WriterTests exposing (suite)

import Elm.Parser.CombineTestUtil exposing (parseFullStringWithNullState)
import Elm.Parser.Declarations exposing (expression)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
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
                    Node empty <|
                        NormalModule
                            { moduleName = Node empty <| [ "A" ]
                            , exposingList = Node empty <| All empty
                            }
                , imports =
                    [ Node empty
                        { moduleName = Node empty <| [ "B" ]
                        , moduleAlias = Nothing
                        , exposingList = Nothing
                        }
                    , Node empty
                        { moduleName = Node empty <| [ "C" ]
                        , moduleAlias = Just (Node empty [ "D" ])
                        , exposingList = Just (Node empty <| All empty)
                        }
                    ]
                , declarations = []
                , comments = []
                }
                    |> Writer.writeFile
                    |> Writer.write
                    |> Expect.equal
                        ("module A exposing (..)\n"
                            ++ "import B  "
                            ++ "\n"
                            ++ "import C as D exposing (..)\n"
                        )
        , describe "Expression"
            [ test "write simple expression" <|
                \() ->
                    (Node empty <|
                        Application (Node empty <| FunctionOrValue [] "abc")
                            [ Node empty <| TupleExpression [] ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "abc ()"
            , test "write qualified expression" <|
                \() ->
                    (Node empty <| FunctionOrValue [ "Foo", "Bar" ] "baz")
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "Foo.Bar.baz"
            , test "Expression.RecordAccessFunction should be parsed then written idempotently" <|
                \() ->
                    let
                        input =
                            "(.spaceEvenly Internal.Style.classes)"
                    in
                    parseFullStringWithNullState input expression
                        |> Maybe.map Writer.writeExpression
                        |> Maybe.map Writer.write
                        |> Expect.equal
                            (Just input)
            , test "regression test for Expression.RecordAccessFunction being written without leading period" <|
                \() ->
                    (Node empty <|
                        Application
                            (Node empty <| FunctionOrValue [ "List" ] "map")
                            [ Node empty <| RecordAccessFunction "name"
                            , Node empty <| FunctionOrValue [] "people"
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "List.map .name people"
            , test "should be able to write strings while keeping their quotes" <|
                \() ->
                    let
                        input : String
                        input =
                            """("a" ++ \"\"\"b\"\"\")"""
                    in
                    parseFullStringWithNullState input expression
                        |> Maybe.map Writer.writeExpression
                        |> Maybe.map Writer.write
                        |> Expect.equal (Just input)
            ]
        , describe "Pattern"
            [ test "write string pattern" <|
                \() ->
                    StringPattern "test"
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\""
            , test "write string pattern containing \"" <|
                \() ->
                    StringPattern "test\""
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "\"test\\\"\""
            ]
        , describe "TypeAnnotation"
            [ test "write simple type" <|
                \() ->
                    Elm.Syntax.TypeAnnotation.Type (Node empty <| ( [], "String" )) []
                        |> Node empty
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "String"
            , test "write qualified type" <|
                \() ->
                    (Node empty <|
                        Elm.Syntax.TypeAnnotation.Type
                            (Node empty <| ( [ "Json", "Decode" ], "Decoder" ))
                            [ Node empty <| Elm.Syntax.TypeAnnotation.Var "a" ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "Json.Decode.Decoder a"
            , test "write type arguments that require parentheses" <|
                \() ->
                    (Node empty <|
                        Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "List" ))
                            [ Node empty <|
                                Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "Dict" ))
                                    [ Node empty <| Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "String" )) []
                                    , Node empty <| Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "Int" )) []
                                    ]
                            ]
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "List (Dict String Int)"
            , test "write type arguments that are functions" <|
                \() ->
                    (Node empty <|
                        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                            (Node empty <|
                                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Node empty <| Elm.Syntax.TypeAnnotation.Var "a")
                                    (Node empty <| Elm.Syntax.TypeAnnotation.Var "b")
                            )
                            (Node empty <| Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "Int" )) [])
                    )
                        |> Writer.writeTypeAnnotation
                        |> Writer.write
                        |> Expect.equal "(a -> b) -> Int"
            ]
        , describe "Declaration"
            [ test "write type declaration > simple constructors" <|
                \() ->
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                (Node empty <| ValueConstructor (Node empty "Foo") [])
                                [ Node empty <| ValueConstructor (Node empty "Bar") [] ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo | Bar"
                            )
            , test "write type declaration > constructors with arguments" <|
                \() ->
                    let
                        listT =
                            Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "List" ))
                                [ Node empty <|
                                    Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "String" )) []
                                ]

                        stringT =
                            Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "String" )) []
                    in
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                (Node empty <| ValueConstructor (Node empty "Foo") [ Node empty listT, Node empty stringT ])
                                [ Node empty <| ValueConstructor (Node empty "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (List String) String | Bar"
                            )
            , test "write type declaration > constructors with functions as arguments" <|
                \() ->
                    let
                        funcT =
                            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                (Node empty <| Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "String" )) [])
                                (Node empty <| Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "Int" )) [])

                        stringT =
                            Elm.Syntax.TypeAnnotation.Type (Node empty ( [], "String" )) []
                    in
                    (Node empty <|
                        CustomTypeDeclaration
                            (Type
                                Nothing
                                (Node empty "Sample")
                                []
                                (Node empty <|
                                    ValueConstructor (Node empty "Foo")
                                        [ Node empty funcT
                                        , Node empty stringT
                                        ]
                                )
                                [ Node empty <| ValueConstructor (Node empty "Bar") []
                                ]
                            )
                    )
                        |> Writer.writeDeclaration
                        |> Writer.write
                        |> Expect.equal
                            ("type Sample \n"
                                ++ "    = Foo (String -> Int) String | Bar"
                            )
            , test "write function with case expression using the right indentations" <|
                \() ->
                    let
                        body : Expression
                        body =
                            CaseExpression
                                (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                    ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                    [ ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                    ]
                                )

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
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
                        body : Expression
                        body =
                            LambdaExpression
                                { firstArg = Node empty (VarPattern_ "myArgument")
                                , restOfArgs = []
                                , expression =
                                    Node empty <|
                                        CaseExpression
                                            (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                                ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                                [ ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                                ]
                                            )
                                }

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
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
                        body : Expression
                        body =
                            TupleExpression
                                [ Node empty <|
                                    LambdaExpression
                                        { firstArg = Node empty (VarPattern_ "myArgument")
                                        , restOfArgs = []
                                        , expression =
                                            Node empty <|
                                                CaseExpression
                                                    (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                                        ( Node empty <| IntPattern 1, Node empty <| FunctionOrValue [] "doSomething" )
                                                        [ ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                                        ]
                                                    )
                                        }
                                ]

                        function : Declaration
                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty body)
                                    )
                                )
                    in
                    Node empty function
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
            , test "regression test for char literals not being escaped" <|
                \() ->
                    ListLiteral
                        [ Node empty (CharLiteral '\\')
                        , Node empty (CharLiteral '"')
                        , Node empty (CharLiteral '\'')
                        , Node empty (CharLiteral '\t')
                        , Node empty (CharLiteral '→')
                        , Node empty (CharLiteral '\u{00A0}')
                        ]
                        |> Node empty
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "['\\\\', '\"', '\\'', '\\\t', '→', '\u{00A0}']"
            , test "regression test for char pattern not being escaped" <|
                \() ->
                    ListPattern
                        [ Node empty (CharPattern '\\')
                        , Node empty (CharPattern '"')
                        , Node empty (CharPattern '\'')
                        , Node empty (CharPattern '\t')
                        , Node empty (CharPattern '→')
                        , Node empty (CharPattern '\u{00A0}')
                        ]
                        |> Node empty
                        |> Writer.writePattern
                        |> Writer.write
                        |> Expect.equal "['\\\\', '\"', '\\'', '\\\t', '→', '\u{00A0}']"
            , test "nested case expressions" <|
                \() ->
                    let
                        body nested =
                            Node empty <|
                                CaseExpression
                                    (CaseBlock (Node empty <| FunctionOrValue [] "someCase")
                                        ( Node empty <| IntPattern 1, nested )
                                        [ ( Node empty <| IntPattern 2, Node empty <| FunctionOrValue [] "doSomethingElse" )
                                        ]
                                    )

                        function =
                            FunctionDeclaration
                                (Function Nothing
                                    Nothing
                                    (Node empty <|
                                        FunctionImplementation
                                            (Node empty <| "functionName")
                                            []
                                            (Node empty
                                                (TupleExpression
                                                    [ Node empty <|
                                                        LambdaExpression
                                                            { firstArg = Node empty (VarPattern_ "myArgument")
                                                            , restOfArgs = []
                                                            , expression =
                                                                body (body (Node empty (TupleExpression [])))
                                                            }
                                                    ]
                                                )
                                            )
                                    )
                                )
                    in
                    Node empty function
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
                    (Node empty <|
                        TupleExpression
                            [ Node empty <| IntegerLiteral 1
                            , Node empty <| IntegerLiteral 2
                            ]
                    )
                        |> Writer.writeExpression
                        |> Writer.write
                        |> Expect.equal "(1, 2)"
            ]
        ]
