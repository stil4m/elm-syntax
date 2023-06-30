module Elm.Parser.Declaration.SignatureTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (Test, describe, test)


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
        ]
