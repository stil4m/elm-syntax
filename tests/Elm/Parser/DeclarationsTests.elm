module Elm.Parser.DeclarationsTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.File as Parser exposing (file)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Module exposing (..)
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
                            , name = "foo"
                            , typeAnnotation = Typed [] "Int" [] emptyRange
                            , range = emptyRange
                            }
                        )
        , test "no spacing signature" <|
            \() ->
                parseFullStringWithNullState "foo:Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = "foo"
                            , typeAnnotation = Typed [] "Int" [] emptyRange
                            , range = emptyRange
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
                            , name = "foo"
                            , typeAnnotation = Typed [] "Int" [] emptyRange
                            , range = emptyRange
                            }
                        )
        , test "on newline signature with colon on start of line" <|
            \() ->
                parseFullStringWithNullState "foo\n:\n Int" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal Nothing
        , test "function declaration" <|
            \() ->
                parseFullStringWithNullState "foo = bar" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = { value = "foo", range = emptyRange }
                            , arguments = []
                            , expression = emptyRanged <| FunctionOrValue "bar"
                            }
                        )
        , test "operator declarations" <|
            \() ->
                parseFullStringWithNullState "(&>) = flip Maybe.andThen" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
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
                        )
        , test "function declaration with args" <|
            \() ->
                parseFullStringWithNullState "inc x = x + 1" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = { value = "inc", range = emptyRange }
                            , arguments = [ VarPattern "x" emptyRange ]
                            , expression =
                                emptyRanged <|
                                    Application
                                        [ emptyRanged <| FunctionOrValue "x"
                                        , emptyRanged <| Operator "+"
                                        , emptyRanged <| Integer 1
                                        ]
                            }
                        )
        , test "some signature" <|
            \() ->
                parseFullStringWithNullState "bar : List ( Int , Maybe m )" Parser.signature
                    |> Maybe.map noRangeSignature
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = "bar"
                            , typeAnnotation =
                                Typed []
                                    "List"
                                    [ Tupled
                                        [ Typed [] "Int" [] emptyRange
                                        , Typed [] "Maybe" [ GenericType "m" emptyRange ] emptyRange
                                        ]
                                        emptyRange
                                    ]
                                    emptyRange
                            , range = emptyRange
                            }
                        )
        , test "function declaration with let" <|
            \() ->
                parseFullStringWithNullState "foo =\n let\n  b = 1\n in\n  b" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = { value = "foo", range = emptyRange }
                            , arguments = []
                            , expression =
                                emptyRanged <|
                                    LetExpression
                                        { declarations =
                                            [ LetFunction
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
                                            ]
                                        , expression = emptyRanged <| FunctionOrValue "b"
                                        }
                            }
                        )
        , test "declaration with record" <|
            \() ->
                parseFullStringWithNullState "main =\n  beginnerProgram { model = 0, view = view, update = update }" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
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
                        )
        , test "update function" <|
            \() ->
                parseFullStringWithNullState "update msg model =\n  case msg of\n    Increment ->\n      model + 1\n\n    Decrement ->\n      model - 1" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
                            { operatorDefinition = False
                            , name = { value = "update", range = emptyRange }
                            , arguments = [ VarPattern "msg" emptyRange, VarPattern "model" emptyRange ]
                            , expression =
                                emptyRanged <|
                                    CaseExpression
                                        { expression = emptyRanged <| FunctionOrValue "msg"
                                        , cases =
                                            [ ( NamedPattern (QualifiedNameRef [] "Increment") [] emptyRange
                                              , emptyRanged <|
                                                    Application
                                                        [ emptyRanged <| FunctionOrValue "model"
                                                        , emptyRanged <| Operator "+"
                                                        , emptyRanged <| Integer 1
                                                        ]
                                              )
                                            , ( NamedPattern (QualifiedNameRef [] "Decrement") [] emptyRange
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
                        )
        , test "port declaration for command" <|
            \() ->
                parseFullStringWithNullState "port parseResponse : ( String, String ) -> Cmd msg" Parser.declaration
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just
                            (PortDeclaration
                                { operatorDefinition = False
                                , name = "parseResponse"
                                , typeAnnotation =
                                    FunctionTypeAnnotation
                                        (Tupled
                                            [ Typed [] "String" [] emptyRange
                                            , Typed [] "String" [] emptyRange
                                            ]
                                            emptyRange
                                        )
                                        (Typed [] "Cmd" [ GenericType "msg" emptyRange ] emptyRange)
                                        emptyRange
                                , range = emptyRange
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                parseFullStringWithNullState "port scroll : (Move -> msg) -> Sub msg" declaration
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            PortDeclaration
                                { operatorDefinition = False
                                , name = "scroll"
                                , typeAnnotation =
                                    FunctionTypeAnnotation
                                        (FunctionTypeAnnotation (Typed [] "Move" [] emptyRange)
                                            (GenericType "msg" emptyRange)
                                            emptyRange
                                        )
                                        (Typed [] "Sub" [ GenericType "msg" emptyRange ] emptyRange)
                                        emptyRange
                                , range = emptyRange
                                }
                        )
        , test "Destructuring declaration" <|
            \() ->
                parseFullStringWithNullState "_ = b" declaration
                    |> Maybe.map noRangeDeclaration
                    |> Expect.equal
                        (Just <|
                            Destructuring
                                (AllPattern emptyRange)
                                (emptyRanged <| FunctionOrValue "b")
                        )
        , test "declaration" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.functionDeclaration
                    |> Maybe.map noRangeFunctionDeclaration
                    |> Expect.equal
                        (Just
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
                        )
        , test "function" <|
            \() ->
                parseFullStringState emptyState "main =\n  text \"Hello, World!\"" Parser.function
                    |> Maybe.map noRangeFunction
                    |> Expect.equal
                        (Just
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
        ]
