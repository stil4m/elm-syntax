module Elm.Parser.DeclarationsTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "DeclarationTests"
        [ test "normal signature" <|
            \() ->
                parseFullStringWithNullState "foo : Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = { value = "foo", range = emptyRange }
                            , typeAnnotation = ( emptyRange, Typed [] "Int" [] )
                            }
                        )
        , test "complex signature" <|
            \() ->
                parseFullStringWithNullState "updateState : (msg -> model -> (model, Cmd msg)) -> SendPort msg model -> msg -> model -> (model, Cmd msg)" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = { range = emptyRange, value = "updateState" }
                            , typeAnnotation =
                                ( emptyRange
                                , FunctionTypeAnnotation
                                    ( emptyRange
                                    , FunctionTypeAnnotation
                                        ( emptyRange
                                        , GenericType "msg"
                                        )
                                        ( emptyRange
                                        , FunctionTypeAnnotation
                                            ( emptyRange
                                            , GenericType "model"
                                            )
                                            ( emptyRange
                                            , Tupled
                                                [ ( emptyRange
                                                  , GenericType "model"
                                                  )
                                                , ( emptyRange
                                                  , Typed [] "Cmd" [ ( emptyRange, GenericType "msg" ) ]
                                                  )
                                                ]
                                            )
                                        )
                                    )
                                    ( emptyRange
                                    , FunctionTypeAnnotation
                                        ( emptyRange
                                        , Typed []
                                            "SendPort"
                                            [ ( emptyRange
                                              , GenericType "msg"
                                              )
                                            , ( emptyRange, GenericType "model" )
                                            ]
                                        )
                                        ( emptyRange
                                        , FunctionTypeAnnotation ( emptyRange, GenericType "msg" ) ( emptyRange, FunctionTypeAnnotation ( emptyRange, GenericType "model" ) ( emptyRange, Tupled [ ( emptyRange, GenericType "model" ), ( emptyRange, Typed [] "Cmd" [ ( emptyRange, GenericType "msg" ) ] ) ] ) )
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
                            { name = { value = "foo", range = emptyRange }
                            , typeAnnotation = ( emptyRange, Typed [] "Int" [] )
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
                            { name = { value = "foo", range = emptyRange }
                            , typeAnnotation = ( emptyRange, Typed [] "Int" [] )
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
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { declaration =
                                    { name = { value = "foo", range = emptyRange }
                                    , arguments = []
                                    , expression = emptyRanged <| FunctionOrValue "bar"
                                    }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "function with case in let" <|
            \() ->
                parseFullStringWithNullState "inc x =\n  let\n    y =\n      case x of\n        True -> z\n    a = b\n  in a" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just
                            (FuncDecl
                                { declaration =
                                    { arguments = [ ( emptyRange, VarPattern "x" ) ]
                                    , expression =
                                        ( emptyRange
                                        , LetExpression
                                            { declarations =
                                                [ ( emptyRange
                                                  , LetFunction
                                                        { declaration =
                                                            { arguments = []
                                                            , expression =
                                                                ( emptyRange
                                                                , CaseExpression
                                                                    { cases =
                                                                        [ ( ( emptyRange, NamedPattern { moduleName = [], name = "True" } [] )
                                                                          , ( emptyRange, FunctionOrValue "z" )
                                                                          )
                                                                        ]
                                                                    , expression = ( emptyRange, FunctionOrValue "x" )
                                                                    }
                                                                )
                                                            , name = { range = emptyRange, value = "y" }
                                                            }
                                                        , documentation = Nothing
                                                        , signature = Nothing
                                                        }
                                                  )
                                                , ( emptyRange
                                                  , LetFunction
                                                        { declaration =
                                                            { arguments = []
                                                            , expression = ( emptyRange, FunctionOrValue "b" )
                                                            , name = { range = emptyRange, value = "a" }
                                                            }
                                                        , documentation = Nothing
                                                        , signature = Nothing
                                                        }
                                                  )
                                                ]
                                            , expression = ( emptyRange, FunctionOrValue "a" )
                                            }
                                        )
                                    , name = { range = emptyRange, value = "inc" }
                                    }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function declaration with args" <|
            \() ->
                parseFullStringWithNullState "inc x = x + 1" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { declaration =
                                    { name = { value = "inc", range = emptyRange }
                                    , arguments = [ ( emptyRange, VarPattern "x" ) ]
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "x"
                                                , emptyRanged <| Operator "+"
                                                , emptyRanged <| Integer 1
                                                ]
                                    }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "some signature" <|
            \() ->
                parseFullStringWithNullState "bar : List ( Int , Maybe m )" Parser.functionSignature
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { name = { value = "bar", range = emptyRange }
                            , typeAnnotation =
                                ( emptyRange
                                , Typed []
                                    "List"
                                    [ ( emptyRange
                                      , Tupled
                                            [ ( emptyRange, Typed [] "Int" [] )
                                            , ( emptyRange, Typed [] "Maybe" [ ( emptyRange, GenericType "m" ) ] )
                                            ]
                                      )
                                    ]
                                )
                            }
                        )
        , test "function declaration with let" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  b = 1\n in\n  b" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    { name = { value = "foo", range = emptyRange }
                                    , arguments = []
                                    , expression =
                                        emptyRanged <|
                                            LetExpression
                                                { declarations =
                                                    [ ( emptyRange
                                                      , LetFunction
                                                            { documentation = Nothing
                                                            , signature = Nothing
                                                            , declaration =
                                                                { name =
                                                                    { value = "b"
                                                                    , range = emptyRange
                                                                    }
                                                                , arguments = []
                                                                , expression = emptyRanged <| Integer 1
                                                                }
                                                            }
                                                      )
                                                    ]
                                                , expression = emptyRanged <| FunctionOrValue "b"
                                                }
                                    }
                                }
                        )
        , test "declaration with record" <|
            \() ->
                parseFullStringWithNullState "main =\n  beginnerProgram { model = 0, view = view, update = update }" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    { name = { value = "main", range = emptyRange }
                                    , arguments = []
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "beginnerProgram"
                                                , emptyRanged <|
                                                    RecordExpr
                                                        [ ( "model", emptyRanged <| Integer 0 )
                                                        , ( "view", emptyRanged <| FunctionOrValue "view" )
                                                        , ( "update", emptyRanged <| FunctionOrValue "update" )
                                                        ]
                                                ]
                                    }
                                }
                        )
        , test "update function" <|
            \() ->
                parseFullStringWithNullState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    { name = { value = "update", range = emptyRange }
                                    , arguments = [ ( emptyRange, VarPattern "msg" ), ( emptyRange, VarPattern "model" ) ]
                                    , expression =
                                        emptyRanged <|
                                            CaseExpression
                                                { expression = emptyRanged <| FunctionOrValue "msg"
                                                , cases =
                                                    [ ( ( emptyRange, NamedPattern (QualifiedNameRef [] "Increment") [] )
                                                      , emptyRanged <|
                                                            Application
                                                                [ emptyRanged <| FunctionOrValue "model"
                                                                , emptyRanged <| Operator "+"
                                                                , emptyRanged <| Integer 1
                                                                ]
                                                      )
                                                    , ( ( emptyRange, NamedPattern (QualifiedNameRef [] "Decrement") [] )
                                                      , emptyRanged <|
                                                            Application
                                                                [ emptyRanged <| FunctionOrValue "model"
                                                                , emptyRanged <| Operator "-"
                                                                , emptyRanged <| Integer 1
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
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { name = { value = "parseResponse", range = emptyRange }
                                , typeAnnotation =
                                    ( emptyRange
                                    , FunctionTypeAnnotation
                                        ( emptyRange
                                        , Tupled
                                            [ ( emptyRange, Typed [] "String" [] )
                                            , ( emptyRange, Typed [] "String" [] )
                                            ]
                                        )
                                        ( emptyRange, Typed [] "Cmd" [ ( emptyRange, GenericType "msg" ) ] )
                                    )
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                parseFullStringWithNullState "port scroll : (Move -> msg) -> Sub msg" declaration
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            PortDeclaration
                                { name = { value = "scroll", range = emptyRange }
                                , typeAnnotation =
                                    ( emptyRange
                                    , FunctionTypeAnnotation
                                        ( emptyRange
                                        , FunctionTypeAnnotation ( emptyRange, Typed [] "Move" [] )
                                            ( emptyRange, GenericType "msg" )
                                        )
                                        ( emptyRange, Typed [] "Sub" [ ( emptyRange, GenericType "msg" ) ] )
                                    )
                                }
                        )
        , test "Destructuring declaration" <|
            \() ->
                parseFullStringWithNullState "_ = b" declaration
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            Destructuring
                                ( emptyRange, AllPattern )
                                (emptyRanged <| FunctionOrValue "b")
                        )
        , test "declaration" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { signature = Nothing
                                , documentation = Nothing
                                , declaration =
                                    { name = { value = "main", range = emptyRange }
                                    , arguments = []
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "text"
                                                , emptyRanged <| Literal "Hello, World!"
                                                ]
                                    }
                                }
                        )
        , test "function" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { name =
                                        { value = "main"
                                        , range = emptyRange
                                        }
                                    , arguments = []
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "text"
                                                , emptyRanged <| Literal "Hello, World!"
                                                ]
                                    }
                                }
                        )
        , test "function starting with multi line comment" <|
            \() ->
                parseFullStringState emptyState "main =\n  {- y -} x" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { name =
                                        { value = "main"
                                        , range = emptyRange
                                        }
                                    , arguments = []
                                    , expression = emptyRanged (FunctionOrValue "x")
                                    }
                                }
                        )
        , test "function with a lot of symbols" <|
            \() ->
                parseFullStringState emptyState "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { declaration =
                                    { arguments = [ ( emptyRange, VarPattern "update" ), ( emptyRange, VarPattern "sendPort" ) ]
                                    , expression =
                                        ( emptyRange
                                        , Application
                                            [ ( emptyRange, FunctionOrValue "curry" )
                                            , ( emptyRange, Operator "<|" )
                                            , ( emptyRange
                                              , ParenthesizedExpression
                                                    ( emptyRange
                                                    , Application
                                                        [ ( emptyRange
                                                          , FunctionOrValue "uncurry"
                                                          )
                                                        , ( emptyRange
                                                          , FunctionOrValue "update"
                                                          )
                                                        ]
                                                    )
                                              )
                                            , ( emptyRange, Operator ">>" )
                                            , ( emptyRange, FunctionOrValue "batchStateCmds" )
                                            , ( emptyRange, FunctionOrValue "sendPort" )
                                            ]
                                        )
                                    , name = { range = emptyRange, value = "updateState" }
                                    }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "Some function" <|
            \() ->
                parseFullStringState emptyState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { declaration =
                                    { arguments =
                                        [ ( emptyRange, VarPattern "msg" )
                                        , ( emptyRange, VarPattern "model" )
                                        ]
                                    , expression =
                                        ( emptyRange
                                        , CaseExpression
                                            { cases =
                                                [ ( ( emptyRange
                                                    , NamedPattern { moduleName = [], name = "Increment" } []
                                                    )
                                                  , ( emptyRange
                                                    , Application
                                                        [ ( emptyRange
                                                          , FunctionOrValue "model"
                                                          )
                                                        , ( emptyRange, Operator "+" )
                                                        , ( emptyRange, Integer 1 )
                                                        ]
                                                    )
                                                  )
                                                , ( ( emptyRange
                                                    , NamedPattern { moduleName = [], name = "Decrement" } []
                                                    )
                                                  , ( emptyRange
                                                    , Application
                                                        [ ( emptyRange
                                                          , FunctionOrValue "model"
                                                          )
                                                        , ( emptyRange, Operator "-" )
                                                        , ( emptyRange, Integer 1 )
                                                        ]
                                                    )
                                                  )
                                                ]
                                            , expression = ( emptyRange, FunctionOrValue "msg" )
                                            }
                                        )
                                    , name = { range = emptyRange, value = "update" }
                                    }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                        )
        , test "some other function" <|
            \() ->
                parseFullStringState emptyState "update : Model\nupdate msg model =\n    msg" Parser.function
                    |> Maybe.map Tuple.second
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { declaration =
                                    { arguments =
                                        [ ( emptyRange, VarPattern "msg" )
                                        , ( emptyRange, VarPattern "model" )
                                        ]
                                    , expression = ( emptyRange, FunctionOrValue "msg" )
                                    , name = { range = emptyRange, value = "update" }
                                    }
                                , documentation = Nothing
                                , signature =
                                    Just
                                        ( emptyRange
                                        , { name = { range = emptyRange, value = "update" }
                                          , typeAnnotation = ( emptyRange, Typed [] "Model" [] )
                                          }
                                        )
                                }
                        )
        ]
