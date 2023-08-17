module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression as Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "DeclarationTests"
        [ test "function declaration" <|
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
                                            , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Expression.Record [])
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
                                            , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern_ "x") ]
                                            , expression =
                                                Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                    (Let
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
                                                                                    (Case
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
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern_ "x") ]
                                        , expression =
                                            Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (FunctionCall
                                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "x"))
                                                    [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Operator "+")
                                                    , Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (IntegerLiteral 1)
                                                    ]
                                                )
                                        }
                                }
                            )
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
                                                (Let
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                            (LetFunction
                                                                { declaration =
                                                                    Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                        { arguments = []
                                                                        , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (IntegerLiteral 1)
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
                                                (Let
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                            (LetDestructuring
                                                                (Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                    (TuplePattern_
                                                                        [ Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (VarPattern_ "b")
                                                                        , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (VarPattern_ "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                    (TupleExpression
                                                                        [ Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (IntegerLiteral 1)
                                                                        , Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (IntegerLiteral 2)
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
                                                (FunctionCall
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (FunctionOrValue [] "beginnerProgram"))
                                                    [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                        (Expression.Record
                                                            [ Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                ( Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                , Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (IntegerLiteral 0)
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
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern_ "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Case
                                                    { firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (FunctionCall
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                [ Node { start = { row = 4, column = 13 }, end = { row = 4, column = 14 } } (Operator "+")
                                                                , Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (IntegerLiteral 1)
                                                                ]
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (FunctionCall
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    [ Node { start = { row = 7, column = 13 }, end = { row = 7, column = 15 } } (Operator "-")
                                                                    , Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (IntegerLiteral 1)
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
                                                (FunctionCall
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (StringLiteral SingleQuote "Hello, World!")
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
                                                (FunctionCall
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (StringLiteral SingleQuote "Hello, World!")
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
                                        { arguments = [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (VarPattern_ "update"), Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (VarPattern_ "sendPort") ]
                                        , expression =
                                            Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                (FunctionCall
                                                    (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "curry"))
                                                    [ Node { start = { row = 1, column = 37 }, end = { row = 1, column = 39 } } (Operator "<|")
                                                    , Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                        (TupleExpression
                                                            [ Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                (FunctionCall
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
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern_ "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Case
                                                    { firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                                                            (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (FunctionCall
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                [ Node { start = { row = 4, column = 13 }, end = { row = 4, column = 14 } } (Operator "+")
                                                                , Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (IntegerLiteral 1)
                                                                ]
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (FunctionCall
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    [ Node { start = { row = 7, column = 13 }, end = { row = 7, column = 15 } } (Operator "-")
                                                                    , Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (IntegerLiteral 1)
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
                                            [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (VarPattern_ "model")
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
