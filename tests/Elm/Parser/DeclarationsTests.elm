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
                            , typeAnnotation = Node.empty <| Typed (Node.empty ( [], "Int" )) []
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
                                                (Node.empty <| GenericType "msg")
                                                (Node.empty <|
                                                    FunctionTypeAnnotation
                                                        (Node.empty <| GenericType "model")
                                                        (Node.empty <|
                                                            Tupled
                                                                [ Node.empty <| GenericType "model"
                                                                , Node.empty <| Typed (Node.empty ( [], "Cmd" )) [ Node empty <| GenericType "msg" ]
                                                                ]
                                                        )
                                                )
                                        )
                                        (Node empty <|
                                            FunctionTypeAnnotation
                                                (Node empty <|
                                                    Typed (Node empty ( [], "SendPort" ))
                                                        [ Node empty <| GenericType "msg"
                                                        , Node empty <| GenericType "model"
                                                        ]
                                                )
                                                (Node empty <|
                                                    FunctionTypeAnnotation (Node empty <| GenericType "msg")
                                                        (Node empty <|
                                                            FunctionTypeAnnotation (Node empty <| GenericType "model")
                                                                (Node empty <|
                                                                    Tupled
                                                                        [ Node empty <| GenericType "model"
                                                                        , Node empty <| Typed (Node empty ( [], "Cmd" )) [ Node empty <| GenericType "msg" ]
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
                            , typeAnnotation = Node empty <| Typed (Node empty ( [], "Int" )) []
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
                            , typeAnnotation = Node empty <| Typed (Node empty ( [], "Int" )) []
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
                                                                                        , cases =
                                                                                            [ ( Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                                              , Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (FunctionOrValue [] "z")
                                                                                              )
                                                                                            ]
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
                                                    [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "x")
                                                    , Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Operator "+")
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
                                    Typed (Node empty ( [], "List" ))
                                        [ Node empty <|
                                            Tupled
                                                [ Node empty <| Typed (Node empty ( [], "Int" )) []
                                                , Node empty <| Typed (Node empty ( [], "Maybe" )) [ Node empty <| GenericType "m" ]
                                                ]
                                        ]
                            }
                        )
        , test "function declaration with let" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  b = 1\n in\n  b" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    Node empty <|
                                        { name = Node empty "foo"
                                        , arguments = []
                                        , expression =
                                            Node empty <|
                                                LetExpression
                                                    { declarations =
                                                        [ Node empty <|
                                                            LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Node empty <|
                                                                        { name = Node empty "b"
                                                                        , arguments = []
                                                                        , expression = Node empty <| Integer 1
                                                                        }
                                                                }
                                                        ]
                                                    , expression = Node empty <| FunctionOrValue [] "b"
                                                    }
                                        }
                                }
                        )
        , test "let destructuring with no spaces around '='" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  (b, c)=(1, 2)\n in\n  b" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    Node empty <|
                                        { name = Node empty "foo"
                                        , arguments = []
                                        , expression =
                                            Node empty <|
                                                LetExpression
                                                    { declarations =
                                                        [ Node empty <|
                                                            LetDestructuring
                                                                (Node empty
                                                                    (TuplePattern
                                                                        [ Node empty (VarPattern "b")
                                                                        , Node empty (VarPattern "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Node empty
                                                                    (TupledExpression
                                                                        [ Node empty (Integer 1)
                                                                        , Node empty (Integer 2)
                                                                        ]
                                                                    )
                                                                )
                                                        ]
                                                    , expression = Node empty <| FunctionOrValue [] "b"
                                                    }
                                        }
                                }
                        )
        , test "declaration with record" <|
            \() ->
                parseFullStringWithNullState "main =\n  beginnerProgram { model = 0, view = view, update = update }" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    Node empty <|
                                        { name = Node empty "main"
                                        , arguments = []
                                        , expression =
                                            Node empty <|
                                                Application
                                                    [ Node empty <| FunctionOrValue [] "beginnerProgram"
                                                    , Node empty <|
                                                        RecordExpr
                                                            [ Node empty ( Node empty "model", Node empty <| Integer 0 )
                                                            , Node empty ( Node empty "view", Node empty <| FunctionOrValue [] "view" )
                                                            , Node empty ( Node empty "update", Node empty <| FunctionOrValue [] "update" )
                                                            ]
                                                    ]
                                        }
                                }
                        )
        , test "update function" <|
            \() ->
                parseFullStringWithNullState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    Node empty <|
                                        { name = Node empty "update"
                                        , arguments = [ Node empty <| VarPattern "msg", Node empty <| VarPattern "model" ]
                                        , expression =
                                            Node empty <|
                                                CaseExpression
                                                    { expression = Node empty <| FunctionOrValue [] "msg"
                                                    , cases =
                                                        [ ( Node empty <| NamedPattern (QualifiedNameRef [] "Increment") []
                                                          , Node empty <|
                                                                Application
                                                                    [ Node empty <| FunctionOrValue [] "model"
                                                                    , Node empty <| Operator "+"
                                                                    , Node empty <| Integer 1
                                                                    ]
                                                          )
                                                        , ( Node empty <| NamedPattern (QualifiedNameRef [] "Decrement") []
                                                          , Node empty <|
                                                                Application
                                                                    [ Node empty <| FunctionOrValue [] "model"
                                                                    , Node empty <| Operator "-"
                                                                    , Node empty <| Integer 1
                                                                    ]
                                                          )
                                                        ]
                                                    }
                                        }
                                }
                        )
        , test "port declaration for command" <|
            \() ->
                parseFullStringWithNullState "port parseResponse : ( String, String ) -> Cmd msg" Parser.declaration
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { name = Node empty "parseResponse"
                                , typeAnnotation =
                                    Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <|
                                                Tupled
                                                    [ Node empty <| Typed (Node empty ( [], "String" )) []
                                                    , Node empty <| Typed (Node empty ( [], "String" )) []
                                                    ]
                                            )
                                            (Node empty <| Typed (Node empty ( [], "Cmd" )) [ Node empty <| GenericType "msg" ])
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                parseFullStringWithNullState "port scroll : (Move -> msg) -> Sub msg" declaration
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            PortDeclaration
                                { name = Node empty "scroll"
                                , typeAnnotation =
                                    Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <|
                                                FunctionTypeAnnotation (Node empty <| Typed (Node empty ( [], "Move" )) [])
                                                    (Node empty <| GenericType "msg")
                                            )
                                            (Node empty <| Typed (Node empty ( [], "Sub" )) [ Node empty <| GenericType "msg" ])
                                }
                        )
        , test "Destructuring declaration" <|
            \() ->
                parseFullStringWithNullState "_ = b" declaration
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            Destructuring
                                (Node empty AllPattern)
                                (Node empty <| FunctionOrValue [] "b")
                        )
        , test "declaration" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    Node empty
                                        { name = Node empty "main"
                                        , arguments = []
                                        , expression =
                                            Node empty <|
                                                Application
                                                    [ Node empty <| FunctionOrValue [] "text"
                                                    , Node empty <| Literal "Hello, World!"
                                                    ]
                                        }
                                }
                        )
        , test "function" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node empty
                                        { name =
                                            Node empty "main"
                                        , arguments = []
                                        , expression =
                                            Node empty <|
                                                Application
                                                    [ Node empty <| FunctionOrValue [] "text"
                                                    , Node empty <| Literal "Hello, World!"
                                                    ]
                                        }
                                }
                        )
        , test "function starting with multi line comment" <|
            \() ->
                parseFullStringState emptyState "main =\n  {- y -} x" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node empty
                                        { name =
                                            Node empty "main"
                                        , arguments = []
                                        , expression = Node.empty (FunctionOrValue [] "x")
                                        }
                                }
                        )
        , test "function with a lot of symbols" <|
            \() ->
                parseFullStringState emptyState "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { declaration =
                                    Node empty
                                        { arguments = [ Node empty <| VarPattern "update", Node empty <| VarPattern "sendPort" ]
                                        , expression =
                                            Node empty <|
                                                Application
                                                    [ Node empty <| FunctionOrValue [] "curry"
                                                    , Node empty <| Operator "<|"
                                                    , Node empty <|
                                                        ParenthesizedExpression
                                                            (Node empty <|
                                                                Application
                                                                    [ Node empty <| FunctionOrValue [] "uncurry"
                                                                    , Node empty <| FunctionOrValue [] "update"
                                                                    ]
                                                            )
                                                    , Node empty <| Operator ">>"
                                                    , Node empty <| FunctionOrValue [] "batchStateCmds"
                                                    , Node empty <| FunctionOrValue [] "sendPort"
                                                    ]
                                        , name = Node empty "updateState"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "Some function" <|
            \() ->
                parseFullStringState emptyState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { declaration =
                                    Node empty
                                        { arguments =
                                            [ Node empty <| VarPattern "msg"
                                            , Node empty <| VarPattern "model"
                                            ]
                                        , expression =
                                            Node empty <|
                                                CaseExpression
                                                    { cases =
                                                        [ ( Node empty <| NamedPattern { moduleName = [], name = "Increment" } []
                                                          , Node empty <|
                                                                Application
                                                                    [ Node empty <| FunctionOrValue [] "model"
                                                                    , Node empty <| Operator "+"
                                                                    , Node empty <| Integer 1
                                                                    ]
                                                          )
                                                        , ( Node empty <| NamedPattern { moduleName = [], name = "Decrement" } []
                                                          , Node empty <|
                                                                Application
                                                                    [ Node empty <| FunctionOrValue [] "model"
                                                                    , Node empty <| Operator "-"
                                                                    , Node empty <| Integer 1
                                                                    ]
                                                          )
                                                        ]
                                                    , expression = Node empty <| FunctionOrValue [] "msg"
                                                    }
                                        , name = Node empty "update"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "some other function" <|
            \() ->
                parseFullStringState emptyState "update : Model\nupdate msg model =\n    msg" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { declaration =
                                    Node empty
                                        { arguments =
                                            [ Node empty <| VarPattern "msg"
                                            , Node empty <| VarPattern "model"
                                            ]
                                        , expression = Node empty <| FunctionOrValue [] "msg"
                                        , name = Node empty "update"
                                        }
                                , documentation = Nothing
                                , signature =
                                    Just
                                        (Node empty <|
                                            { name = Node empty "update"
                                            , typeAnnotation = Node empty <| Typed (Node empty ( [], "Model" )) []
                                            }
                                        )
                                }
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
