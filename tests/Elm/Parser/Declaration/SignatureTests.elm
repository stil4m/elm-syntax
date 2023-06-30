module Elm.Parser.Declaration.SignatureTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil
import Elm.Parser.Declarations as Declarations
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "DeclarationTests"
        [ test "normal signature" <|
            \() ->
                "foo : Int"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                            , typeAnnotation =
                                Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } }
                                    (Type (Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } ( [], "Int" )) [])
                            }
                        )
        , test "complex signature" <|
            \() ->
                "updateState : (msg -> model -> (model, Cmd msg)) -> SendPort msg model -> msg -> model -> (model, Cmd msg)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 107 } }
                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                            , typeAnnotation =
                                Node { start = { row = 1, column = 15 }, end = { row = 1, column = 107 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 49 } }
                                            (FunctionTypeAnnotation (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 19 } } (Var "msg"))
                                                (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 48 } }
                                                    (FunctionTypeAnnotation (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } } (Var "model"))
                                                        (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 48 } }
                                                            (Tuple
                                                                [ Node { start = { row = 1, column = 33 }, end = { row = 1, column = 38 } } (Var "model")
                                                                , Node { start = { row = 1, column = 40 }, end = { row = 1, column = 47 } }
                                                                    (Type (Node { start = { row = 1, column = 40 }, end = { row = 1, column = 43 } } ( [], "Cmd" ))
                                                                        [ Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } (Var "msg") ]
                                                                    )
                                                                ]
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (Node { start = { row = 1, column = 53 }, end = { row = 1, column = 107 } }
                                            (FunctionTypeAnnotation
                                                (Node { start = { row = 1, column = 53 }, end = { row = 1, column = 71 } }
                                                    (Type (Node { start = { row = 1, column = 53 }, end = { row = 1, column = 61 } } ( [], "SendPort" ))
                                                        [ Node { start = { row = 1, column = 62 }, end = { row = 1, column = 65 } } (Var "msg"), Node { start = { row = 1, column = 66 }, end = { row = 1, column = 71 } } (Var "model") ]
                                                    )
                                                )
                                                (Node { start = { row = 1, column = 75 }, end = { row = 1, column = 107 } }
                                                    (FunctionTypeAnnotation (Node { start = { row = 1, column = 75 }, end = { row = 1, column = 78 } } (Var "msg"))
                                                        (Node { start = { row = 1, column = 82 }, end = { row = 1, column = 107 } }
                                                            (FunctionTypeAnnotation (Node { start = { row = 1, column = 82 }, end = { row = 1, column = 87 } } (Var "model"))
                                                                (Node { start = { row = 1, column = 91 }, end = { row = 1, column = 107 } }
                                                                    (Tuple
                                                                        [ Node { start = { row = 1, column = 92 }, end = { row = 1, column = 97 } } (Var "model")
                                                                        , Node { start = { row = 1, column = 99 }, end = { row = 1, column = 106 } }
                                                                            (Type (Node { start = { row = 1, column = 99 }, end = { row = 1, column = 102 } } ( [], "Cmd" ))
                                                                                [ Node { start = { row = 1, column = 103 }, end = { row = 1, column = 106 } } (Var "msg") ]
                                                                            )
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                            }
                        )
        , test "no spacing signature" <|
            \() ->
                "foo:Int"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                            , typeAnnotation =
                                Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } }
                                    (Type (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } ( [], "Int" )) [])
                            }
                        )
        , test "on newline signature with wrong indent " <|
            \() ->
                "foo :\nInt"
                    |> expectInvalid
        , test "on newline signature with good indent" <|
            \() ->
                "foo :\n Int"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 5 } }
                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                            , typeAnnotation =
                                Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } }
                                    (Type (Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } ( [], "Int" )) [])
                            }
                        )
        , test "on newline signature with colon on start of line" <|
            \() ->
                "foo\n:\n Int"
                    |> expectInvalid
        ]


expectAst : Node Signature -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst Declarations.signature


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid Declarations.signature
