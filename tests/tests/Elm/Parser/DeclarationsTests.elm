module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DeconstructPattern exposing (DeconstructPattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
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
                            { name = Node emptyRange "foo"
                            , typeAnnotation = Node emptyRange <| Type (Node emptyRange ( [], "Int" )) []
                            }
                        )
        , test "complex signature" <|
            \() ->
                parseFullStringWithNullState "updateState : (msg -> model -> (model, Cmd msg)) -> SendPort msg model -> msg -> model -> (model, Cmd msg)" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node emptyRange "updateState"
                            , typeAnnotation =
                                Node emptyRange <|
                                    FunctionTypeAnnotation
                                        (Node emptyRange <|
                                            FunctionTypeAnnotation
                                                (Node emptyRange <| Var "msg")
                                                (Node emptyRange <|
                                                    FunctionTypeAnnotation
                                                        (Node emptyRange <| Var "model")
                                                        (Node emptyRange <|
                                                            Tuple
                                                                [ Node emptyRange <| Var "model"
                                                                , Node emptyRange <| Type (Node emptyRange ( [], "Cmd" )) [ Node emptyRange <| Var "msg" ]
                                                                ]
                                                        )
                                                )
                                        )
                                        (Node emptyRange <|
                                            FunctionTypeAnnotation
                                                (Node emptyRange <|
                                                    Type (Node emptyRange ( [], "SendPort" ))
                                                        [ Node emptyRange <| Var "msg"
                                                        , Node emptyRange <| Var "model"
                                                        ]
                                                )
                                                (Node emptyRange <|
                                                    FunctionTypeAnnotation (Node emptyRange <| Var "msg")
                                                        (Node emptyRange <|
                                                            FunctionTypeAnnotation (Node emptyRange <| Var "model")
                                                                (Node emptyRange <|
                                                                    Tuple
                                                                        [ Node emptyRange <| Var "model"
                                                                        , Node emptyRange <| Type (Node emptyRange ( [], "Cmd" )) [ Node emptyRange <| Var "msg" ]
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
                            { name = Node emptyRange "foo"
                            , typeAnnotation = Node emptyRange <| Type (Node emptyRange ( [], "Int" )) []
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
                            { name = Node emptyRange "foo"
                            , typeAnnotation = Node emptyRange <| Type (Node emptyRange ( [], "Int" )) []
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
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { declaration =
                                    Node emptyRange
                                        { name = Node emptyRange "foo"
                                        , arguments = []
                                        , expression = Node emptyRange <| FunctionOrValue [] "bar"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "function declaration with empty record" <|
            \() ->
                parseFullStringWithNullState "foo = {}" Parser.function
                    |> Maybe.map (Node.value >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node emptyRange
                                        { name = Node emptyRange "foo"
                                        , arguments = []
                                        , expression = Node emptyRange <| RecordExpr []
                                        }
                                }
                        )
        , test "function with case in let" <|
            \() ->
                parseFullStringWithNullState "inc x =\n  let\n    y =\n      case x of\n        True -> z\n    a = b\n  in a" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just
                            (FunctionDeclaration
                                { declaration =
                                    Node emptyRange <|
                                        { arguments = [ Node emptyRange <| VarPattern_ "x" ]
                                        , expression =
                                            Node emptyRange <|
                                                LetExpression
                                                    { declarations =
                                                        [ Node emptyRange <|
                                                            LetFunction
                                                                { declaration =
                                                                    Node emptyRange <|
                                                                        { arguments = []
                                                                        , expression =
                                                                            Node emptyRange <|
                                                                                CaseExpression
                                                                                    { firstCase =
                                                                                        ( Node emptyRange <| NamedPattern { moduleName = [], name = "True" } []
                                                                                        , Node emptyRange <| FunctionOrValue [] "z"
                                                                                        )
                                                                                    , restOfCases = []
                                                                                    , expression = Node emptyRange <| FunctionOrValue [] "x"
                                                                                    }
                                                                        , name = Node emptyRange "y"
                                                                        }
                                                                , documentation = Nothing
                                                                , signature = Nothing
                                                                }
                                                        , Node emptyRange <|
                                                            LetFunction
                                                                { declaration =
                                                                    Node emptyRange <|
                                                                        { arguments = []
                                                                        , expression = Node emptyRange <| FunctionOrValue [] "b"
                                                                        , name = Node emptyRange "a"
                                                                        }
                                                                , documentation = Nothing
                                                                , signature = Nothing
                                                                }
                                                        ]
                                                    , expression = Node emptyRange <| FunctionOrValue [] "a"
                                                    }
                                        , name = Node emptyRange "inc"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function declaration with args" <|
            \() ->
                parseFullStringWithNullState "inc x = x + 1" Parser.function
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FunctionDeclaration
                                { declaration =
                                    Node emptyRange <|
                                        { name = Node emptyRange "inc"
                                        , arguments = [ Node emptyRange <| VarPattern_ "x" ]
                                        , expression =
                                            Node emptyRange <|
                                                Application
                                                    (Node emptyRange <| FunctionOrValue [] "x")
                                                    [ Node emptyRange <| Operator "+"
                                                    , Node emptyRange <| Integer 1
                                                    ]
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "some signature" <|
            \() ->
                parseFullStringWithNullState "bar : List ( Int , Maybe m )" Parser.functionSignature
                    |> Maybe.map Node.value
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = Node emptyRange "bar"
                            , typeAnnotation =
                                Node emptyRange <|
                                    Type (Node emptyRange ( [], "List" ))
                                        [ Node emptyRange <|
                                            Tuple
                                                [ Node emptyRange <| Type (Node emptyRange ( [], "Int" )) []
                                                , Node emptyRange <| Type (Node emptyRange ( [], "Maybe" )) [ Node emptyRange <| Var "m" ]
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
                                    Node emptyRange <|
                                        { name = Node emptyRange "foo"
                                        , arguments = []
                                        , expression =
                                            Node emptyRange <|
                                                LetExpression
                                                    { declarations =
                                                        [ Node emptyRange <|
                                                            LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Node emptyRange <|
                                                                        { name = Node emptyRange "b"
                                                                        , arguments = []
                                                                        , expression = Node emptyRange <| Integer 1
                                                                        }
                                                                }
                                                        ]
                                                    , expression = Node emptyRange <| FunctionOrValue [] "b"
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
                                    Node emptyRange <|
                                        { name = Node emptyRange "main"
                                        , arguments = []
                                        , expression =
                                            Node emptyRange <|
                                                Application
                                                    (Node emptyRange <| FunctionOrValue [] "beginnerProgram")
                                                    [ Node emptyRange <|
                                                        RecordExpr
                                                            [ Node emptyRange ( Node emptyRange "model", Node emptyRange <| Integer 0 )
                                                            , Node emptyRange ( Node emptyRange "view", Node emptyRange <| FunctionOrValue [] "view" )
                                                            , Node emptyRange ( Node emptyRange "update", Node emptyRange <| FunctionOrValue [] "update" )
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
                                    Node emptyRange <|
                                        { name = Node emptyRange "update"
                                        , arguments =
                                            [ Node emptyRange <| VarPattern_ "msg"
                                            , Node emptyRange <| VarPattern_ "model"
                                            ]
                                        , expression =
                                            Node emptyRange <|
                                                CaseExpression
                                                    { expression = Node emptyRange <| FunctionOrValue [] "msg"
                                                    , firstCase =
                                                        ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "Increment") []
                                                        , Node emptyRange <|
                                                            Application
                                                                (Node emptyRange <| FunctionOrValue [] "model")
                                                                [ Node emptyRange <| Operator "+"
                                                                , Node emptyRange <| Integer 1
                                                                ]
                                                        )
                                                    , restOfCases =
                                                        [ ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "Decrement") []
                                                          , Node emptyRange <|
                                                                Application
                                                                    (Node emptyRange <| FunctionOrValue [] "model")
                                                                    [ Node emptyRange <| Operator "-"
                                                                    , Node emptyRange <| Integer 1
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
                                { documentation = Nothing
                                , signature =
                                    Node emptyRange <|
                                        { name = Node emptyRange "parseResponse"
                                        , typeAnnotation =
                                            Node emptyRange <|
                                                FunctionTypeAnnotation
                                                    (Node emptyRange <|
                                                        Tuple
                                                            [ Node emptyRange <| Type (Node emptyRange ( [], "String" )) []
                                                            , Node emptyRange <| Type (Node emptyRange ( [], "String" )) []
                                                            ]
                                                    )
                                                    (Node emptyRange <| Type (Node emptyRange ( [], "Cmd" )) [ Node emptyRange <| Var "msg" ])
                                        }
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
                                { documentation = Nothing
                                , signature =
                                    Node emptyRange <|
                                        { name = Node emptyRange "scroll"
                                        , typeAnnotation =
                                            Node emptyRange <|
                                                FunctionTypeAnnotation
                                                    (Node emptyRange <|
                                                        FunctionTypeAnnotation (Node emptyRange <| Type (Node emptyRange ( [], "Move" )) [])
                                                            (Node emptyRange <| Var "msg")
                                                    )
                                                    (Node emptyRange <| Type (Node emptyRange ( [], "Sub" )) [ Node emptyRange <| Var "msg" ])
                                        }
                                }
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
                                    Node emptyRange
                                        { name = Node emptyRange "main"
                                        , arguments = []
                                        , expression =
                                            Node emptyRange <|
                                                Application
                                                    (Node emptyRange <| FunctionOrValue [] "text")
                                                    [ Node emptyRange <| Literal "Hello, World!" ]
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
                                    Node emptyRange
                                        { name =
                                            Node emptyRange "main"
                                        , arguments = []
                                        , expression =
                                            Node emptyRange <|
                                                Application
                                                    (Node emptyRange <| FunctionOrValue [] "text")
                                                    [ Node emptyRange <| Literal "Hello, World!" ]
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
                                    Node emptyRange
                                        { name =
                                            Node emptyRange "main"
                                        , arguments = []
                                        , expression = emptyRanged (FunctionOrValue [] "x")
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
                                    Node emptyRange
                                        { arguments =
                                            [ Node emptyRange <| VarPattern_ "update"
                                            , Node emptyRange <| VarPattern_ "sendPort"
                                            ]
                                        , expression =
                                            Node emptyRange <|
                                                Application
                                                    (Node emptyRange <| FunctionOrValue [] "curry")
                                                    [ Node emptyRange <| Operator "<|"
                                                    , Node emptyRange <|
                                                        ParenthesizedExpression
                                                            (Node emptyRange <|
                                                                Application
                                                                    (Node emptyRange <| FunctionOrValue [] "uncurry")
                                                                    [ Node emptyRange <| FunctionOrValue [] "update" ]
                                                            )
                                                    , Node emptyRange <| Operator ">>"
                                                    , Node emptyRange <| FunctionOrValue [] "batchStateCmds"
                                                    , Node emptyRange <| FunctionOrValue [] "sendPort"
                                                    ]
                                        , name = Node emptyRange "updateState"
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
                                    Node emptyRange
                                        { arguments =
                                            [ Node emptyRange <| VarPattern_ "msg"
                                            , Node emptyRange <| VarPattern_ "model"
                                            ]
                                        , expression =
                                            Node emptyRange <|
                                                CaseExpression
                                                    { firstCase =
                                                        ( Node emptyRange <| NamedPattern { moduleName = [], name = "Increment" } []
                                                        , Node emptyRange <|
                                                            Application
                                                                (Node emptyRange <| FunctionOrValue [] "model")
                                                                [ Node emptyRange <| Operator "+"
                                                                , Node emptyRange <| Integer 1
                                                                ]
                                                        )
                                                    , restOfCases =
                                                        [ ( Node emptyRange <| NamedPattern { moduleName = [], name = "Decrement" } []
                                                          , Node emptyRange <|
                                                                Application
                                                                    (Node emptyRange <| FunctionOrValue [] "model")
                                                                    [ Node emptyRange <| Operator "-"
                                                                    , Node emptyRange <| Integer 1
                                                                    ]
                                                          )
                                                        ]
                                                    , expression = Node emptyRange <| FunctionOrValue [] "msg"
                                                    }
                                        , name = Node emptyRange "update"
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
                                    Node emptyRange
                                        { arguments =
                                            [ Node emptyRange <| VarPattern_ "msg"
                                            , Node emptyRange <| VarPattern_ "model"
                                            ]
                                        , expression = Node emptyRange <| FunctionOrValue [] "msg"
                                        , name = Node emptyRange "update"
                                        }
                                , documentation = Nothing
                                , signature =
                                    Just
                                        (Node emptyRange <|
                                            { name = Node emptyRange "update"
                                            , typeAnnotation = Node emptyRange <| Type (Node emptyRange ( [], "Model" )) []
                                            }
                                        )
                                }
                        )
        ]
