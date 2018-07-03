module Elm.Parser.DeclarationsTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Base exposing (VariablePointer)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
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
                            { operatorDefinition = False
                            , name = VariablePointer "foo" emptyRange
                            , typeAnnotation = ( emptyRange, Typed [] "Int" [] )
                            }
                        )
        , test "no spacing signature" <|
            \() ->
                parseFullStringWithNullState "foo:Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = VariablePointer "foo" emptyRange
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
                            { operatorDefinition = False
                            , name = VariablePointer "foo" emptyRange
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "foo", range = emptyRange }
                                    , arguments = []
                                    , expression = emptyRanged <| FunctionOrValue "bar"
                                    }
                                }
                        )
        , test "function declaration with empty record" <|
            \() ->
                parseFullStringWithNullState "foo = {}" Parser.function
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "foo", range = emptyRange }
                                    , arguments = []
                                    , expression = emptyRanged <| RecordExpr []
                                    }
                                }
                        )
        , test "operator declarations" <|
            \() ->
                parseFullStringWithNullState "(&>) = flip Maybe.andThen" Parser.function
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = True
                                    , name = { value = "&>", range = emptyRange }
                                    , arguments = []
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "flip"
                                                , emptyRanged <| QualifiedExpr [ "Maybe" ] "andThen"
                                                ]
                                    }
                                }
                        )
        , test "function declaration with args" <|
            \() ->
                parseFullStringWithNullState "inc x = x + 1" Parser.function
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "inc", range = emptyRange }
                                    , arguments = [ ( emptyRange, VarPattern "x" ) ]
                                    , expression =
                                        emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "x"
                                                , emptyRanged <| Operator "+"
                                                , emptyRanged <| Integer 1
                                                ]
                                    }
                                }
                        )
        , test "some signature" <|
            \() ->
                parseFullStringWithNullState "bar : List ( Int , Maybe m )" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = VariablePointer "bar" emptyRange
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "foo", range = emptyRange }
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
                                                                { operatorDefinition = False
                                                                , name =
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "main", range = emptyRange }
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "update", range = emptyRange }
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { operatorDefinition = False
                                , name = VariablePointer "parseResponse" emptyRange
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            PortDeclaration
                                { operatorDefinition = False
                                , name = VariablePointer "scroll" emptyRange
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            Destructuring
                                ( emptyRange, AllPattern )
                                (emptyRanged <| FunctionOrValue "b")
                        )
        , test "declaration" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "main", range = emptyRange }
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name =
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
                    |> Maybe.map (Tuple.second >> noRangeDeclaration)
                    |> Expect.equal
                        (Just <|
                            FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name =
                                        { value = "main"
                                        , range = emptyRange
                                        }
                                    , arguments = []
                                    , expression = emptyRanged (FunctionOrValue "x")
                                    }
                                }
                        )
        , test "function with case should not be eager on the whitespace" <|
            \() ->
                parseFullStringState emptyState "foo x =\n    case x of\n        False ->\n            x\n        _ -> not x\n\n\n" Parser.function
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (FuncDecl
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    { operatorDefinition = False
                                    , name = { value = "foo", range = { start = { row = 0, column = 0 }, end = { row = 0, column = 3 } } }
                                    , arguments = [ ( { start = { row = 0, column = 4 }, end = { row = 0, column = 5 } }, VarPattern "x" ) ]
                                    , expression =
                                        ( { start = { row = 1, column = 4 }, end = { row = 4, column = 18 } }
                                        , CaseExpression
                                            { expression =
                                                ( { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                , FunctionOrValue "x"
                                                )
                                            , cases =
                                                [ ( ( { start = { row = 2, column = 8 }, end = { row = 2, column = 13 } }
                                                    , NamedPattern { moduleName = [], name = "False" } []
                                                    )
                                                  , ( { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }, FunctionOrValue "x" )
                                                  )
                                                , ( ( { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } }
                                                    , AllPattern
                                                    )
                                                  , ( { start = { row = 4, column = 13 }, end = { row = 4, column = 18 } }
                                                    , Application
                                                        [ ( { start = { row = 4, column = 13 }, end = { row = 4, column = 16 } }, FunctionOrValue "not" )
                                                        , ( { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } }, FunctionOrValue "x" )
                                                        ]
                                                    )
                                                  )
                                                ]
                                            }
                                        )
                                    }
                                }
                            )
                        )
        ]
