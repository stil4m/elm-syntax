module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "DeclarationTests"
        [ test "normal signature" <|
            \() ->
                parseFullStringWithNullState "foo : Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node.empty "foo"
                            , typeAnnotation = Node.empty <| Type (Node.empty ( [], "Int" )) []
                            }
                        )
        , test "complex signature" <|
            \() ->
                parseFullStringWithNullState "updateState : (msg -> model -> (model, Cmd msg)) -> SendPort msg model -> msg -> model -> (model, Cmd msg)" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node.empty "updateState"
                            , typeAnnotation =
                                Node.empty <|
                                    FunctionTypeAnnotation
                                        (Node.empty <|
                                            FunctionTypeAnnotation
                                                (Node.empty <| Var "msg")
                                                (Node.empty <|
                                                    FunctionTypeAnnotation
                                                        (Node.empty <| Var "model")
                                                        (Node.empty <|
                                                            Tuple
                                                                [ Node.empty <| Var "model"
                                                                , Node.empty <| Type (Node.empty ( [], "Cmd" )) [ Node empty <| Var "msg" ]
                                                                ]
                                                        )
                                                )
                                        )
                                        (Node empty <|
                                            FunctionTypeAnnotation
                                                (Node empty <|
                                                    Type (Node empty ( [], "SendPort" ))
                                                        [ Node empty <| Var "msg"
                                                        , Node empty <| Var "model"
                                                        ]
                                                )
                                                (Node empty <|
                                                    FunctionTypeAnnotation (Node empty <| Var "msg")
                                                        (Node empty <|
                                                            FunctionTypeAnnotation (Node empty <| Var "model")
                                                                (Node empty <|
                                                                    Tuple
                                                                        [ Node empty <| Var "model"
                                                                        , Node empty <| Type (Node empty ( [], "Cmd" )) [ Node empty <| Var "msg" ]
                                                                        ]
                                                                )
                                                        )
                                                )
                                        )
                            }
                        )
        , test "no spacing signature" <|
            \() ->
                parseFullStringWithNullState "foo:Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node empty "foo"
                            , typeAnnotation = Node empty <| Type (Node empty ( [], "Int" )) []
                            }
                        )
        , test "on newline signature with wrong indent " <|
            \() ->
                parseFullStringWithNullState "foo :\nInt" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal Nothing
        , test "on newline signature with good indent" <|
            \() ->
                parseFullStringWithNullState "foo :\n Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node empty "foo"
                            , typeAnnotation = Node empty <| Type (Node empty ( [], "Int" )) []
                            }
                        )
        , test "on newline signature with colon on start of line" <|
            \() ->
                parseFullStringWithNullState "foo\n:\n Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal Nothing
        , test "function declaration" <|
            \() ->
                parseFullStringWithNullState "foo = bar" Parser.function
                    |> Expect.equal
                        (Just <|
                            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (FunctionDeclaration
                                    { declaration =
                                        Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "bar")
                                            }
                                    , documentation = Nothing
                                    , signature = Nothing
                                    }
                                )
                        )
        , test "function declaration with empty record" <|
            \() ->
                parseFullStringWithNullState "foo = {}" Parser.function
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                (FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                            { arguments = []
                                            , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (RecordExpr [])
                                            }
                                    }
                                )
                            )
                        )
        , test "function with case in let" <|
            \() ->
                parseFullStringWithNullState "inc x =\n  let\n    y =\n      case x of\n        True -> z\n    a = b\n  in a" Parser.function
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                (FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                            , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                                            , expression =
                                                Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                    (LetExpression
                                                        { declarations =
                                                            [ Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                (LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                            { name = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } "y"
                                                                            , arguments = []
                                                                            , expression =
                                                                                Node { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                    (CaseExpression
                                                                                        { expression = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "x")
                                                                                        , firstCase =
                                                                                            ( Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                                            , Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (FunctionOrValue [] "z")
                                                                                            )
                                                                                        , restOfCases = []
                                                                                        }
                                                                                    )
                                                                            }
                                                                    }
                                                                )
                                                            , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                (LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                            { name = Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } "a"
                                                                            , arguments = []
                                                                            , expression = Node { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } } (FunctionOrValue [] "b")
                                                                            }
                                                                    }
                                                                )
                                                            ]
                                                        , expression = Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (FunctionOrValue [] "a")
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                        )
        , test "function declaration with args" <|
            \() ->
                parseFullStringWithNullState "inc x = x + 1" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                                        , expression =
                                            Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (Application
                                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "x"))
                                                    [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Operator "+")
                                                    , Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Integer 1)
                                                    ]
                                                )
                                        }
                                }
                            )
                        )
        , test "some signature" <|
            \() ->
                parseFullStringWithNullState "bar : List ( Int , Maybe m )" Parser.functionSignature
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node empty "bar"
                            , typeAnnotation =
                                Node empty <|
                                    Type (Node empty ( [], "List" ))
                                        [ Node empty <|
                                            Tuple
                                                [ Node empty <| Type (Node empty ( [], "Int" )) []
                                                , Node empty <| Type (Node empty ( [], "Maybe" )) [ Node empty <| Var "m" ]
                                                ]
                                        ]
                            }
                        )
        , test "function declaration with let" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  b = 1\n in\n  b" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                            (LetFunction
                                                                { declaration =
                                                                    Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                        { arguments = []
                                                                        , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Integer 1)
                                                                        , name = Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                        }
                                                                , documentation = Nothing
                                                                , signature = Nothing
                                                                }
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "let destructuring with no spaces around '='" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  (b, c)=(1, 2)\n in\n  b" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                            (LetDestructuring
                                                                (Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                    (TuplePattern
                                                                        [ Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (VarPattern "b")
                                                                        , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (VarPattern "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                    (TupleExpression
                                                                        [ Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Integer 1)
                                                                        , Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (Integer 2)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "declaration with record" <|
            \() ->
                parseFullStringWithNullState "main =\n  beginnerProgram { model = 0, view = view, update = update }" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (FunctionOrValue [] "beginnerProgram"))
                                                    [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                        (RecordExpr
                                                            [ Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                ( Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                , Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (Integer 0)
                                                                )
                                                            , Node { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                ( Node { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } } "view"
                                                                , Node { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } } (FunctionOrValue [] "view")
                                                                )
                                                            , Node { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                ( Node { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } } "update"
                                                                , Node { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } } (FunctionOrValue [] "update")
                                                                )
                                                            ]
                                                        )
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "update function" <|
            \() ->
                parseFullStringWithNullState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (CaseExpression
                                                    { firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (Application
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                [ Node { start = { row = 4, column = 13 }, end = { row = 4, column = 14 } } (Operator "+")
                                                                , Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Integer 1)
                                                                ]
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Application
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    [ Node { start = { row = 7, column = 13 }, end = { row = 7, column = 15 } } (Operator "-")
                                                                    , Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Integer 1)
                                                                    ]
                                                                )
                                                          )
                                                        ]
                                                    , expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "port declaration for command" <|
            \() ->
                parseFullStringWithNullState "port parseResponse : ( String, String ) -> Cmd msg" Parser.declaration
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { documentation = Nothing
                                , signature =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 51 } } <|
                                        { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 19 } } "parseResponse"
                                        , typeAnnotation =
                                            Node { start = { row = 1, column = 22 }, end = { row = 1, column = 51 } }
                                                (FunctionTypeAnnotation
                                                    (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 40 } }
                                                        (Tuple
                                                            [ Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } (Type (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                            , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } }
                                                                (Type
                                                                    (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } } ( [], "String" ))
                                                                    []
                                                                )
                                                            ]
                                                        )
                                                    )
                                                    (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 51 } }
                                                        (Type
                                                            (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } ( [], "Cmd" ))
                                                            [ Node { start = { row = 1, column = 48 }, end = { row = 1, column = 51 } } (Var "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                parseFullStringWithNullState "port scroll : (Move -> msg) -> Sub msg" declaration
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { documentation = Nothing
                                , signature =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 39 } } <|
                                        { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } "scroll"
                                        , typeAnnotation =
                                            Node { start = { row = 1, column = 15 }, end = { row = 1, column = 39 } }
                                                (FunctionTypeAnnotation
                                                    (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 28 } }
                                                        (FunctionTypeAnnotation
                                                            (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                                (Type (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } ( [], "Move" )) [])
                                                            )
                                                            (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Var "msg"))
                                                        )
                                                    )
                                                    (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 39 } }
                                                        (Type (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 35 } } ( [], "Sub" ))
                                                            [ Node { start = { row = 1, column = 36 }, end = { row = 1, column = 39 } } (Var "msg")
                                                            ]
                                                        )
                                                    )
                                                )
                                        }
                                }
                            )
                        )
        , test "declaration" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Literal "Hello, World!")
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Literal "Hello, World!")
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function starting with multi line comment" <|
            \() ->
                parseFullStringState emptyState "main =\n  {- y -} x" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                        { arguments = []
                                        , expression = Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (FunctionOrValue [] "x")
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                }
                            )
                        )
        , test "function with a lot of symbols" <|
            \() ->
                parseFullStringState emptyState "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                        { arguments = [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (VarPattern "update"), Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (VarPattern "sendPort") ]
                                        , expression =
                                            Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                (Application
                                                    (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "curry"))
                                                    [ Node { start = { row = 1, column = 37 }, end = { row = 1, column = 39 } } (Operator "<|")
                                                    , Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                        (TupleExpression
                                                            [ Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                (Application
                                                                    (Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (FunctionOrValue [] "uncurry"))
                                                                    [ Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (FunctionOrValue [] "update")
                                                                    ]
                                                                )
                                                            ]
                                                        )
                                                    , Node { start = { row = 1, column = 57 }, end = { row = 1, column = 59 } } (Operator ">>")
                                                    , Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (FunctionOrValue [] "batchStateCmds")
                                                    , Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (FunctionOrValue [] "sendPort")
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "Some function" <|
            \() ->
                parseFullStringState emptyState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (CaseExpression
                                                    { firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                                                            (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (Application
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                [ Node { start = { row = 4, column = 13 }, end = { row = 4, column = 14 } } (Operator "+")
                                                                , Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Integer 1)
                                                                ]
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Application
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    [ Node { start = { row = 7, column = 13 }, end = { row = 7, column = 15 } } (Operator "-")
                                                                    , Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Integer 1)
                                                                    ]
                                                                )
                                                          )
                                                        ]
                                                    , expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "some other function" <|
            \() ->
                parseFullStringState emptyState "update : Model\nupdate msg model =\n    msg" Parser.function
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                        { arguments =
                                            [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (FunctionOrValue [] "msg")
                                        , name = Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature =
                                    Just
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , typeAnnotation = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Type (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                            }
                                        )
                                }
                            )
                        )
        , test "regression test for disallowing ( +)" <|
            \() ->
                parseFullStringState emptyState "a = ( +)" Parser.function
                    |> Expect.equal Nothing
        , test "regression test for disallowing (+ )" <|
            \() ->
                parseFullStringState emptyState "a = (+ )" Parser.function
                    |> Expect.equal Nothing
        ]
