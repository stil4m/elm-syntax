module Elm.ProcessingTests exposing (functionWithDocs, functionWithDocsAndSignature, functionWithMultiLineCommentAsDoc, functionWithSingleLineCommentAsDoc, postProcessInfixOperators, suite, typeAliasWithDocumentation)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Parser exposing (DeadEnd)
import Test exposing (..)


functionWithDocs : ( String, String, File )
functionWithDocs =
    ( "functionWithDocs"
    , "\nmodule Bar exposing (..)\n\n"
        ++ "{-| The docs\n"
        ++ "-}\n"
        ++ "bar = 1\n"
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList =
                        Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <|
                            All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Just (Node { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } } "{-| The docs\n-}")
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 8, row = 5 }, start = { column = 1, row = 5 } }
                            { name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 5, column = 7 }, end = { row = 5, column = 8 } } <| Integer 1
                            }
                    }
            ]
      , comments = []
      }
    )


functionWithDocsAndSignature : ( String, String, File )
functionWithDocsAndSignature =
    ( "functionWithDocsAndSignature"
    , "\nmodule Bar exposing (..)\n\n"
        ++ "{-| The docs\n"
        ++ "-}\n"
        ++ "bar : Int\n"
        ++ "bar = 1\n"
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 5, column = 1 }, end = { row = 6, column = 8 } } <|
                FunctionDeclaration
                    { documentation =
                        Just <| Node { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } } "{-| The docs\n-}"
                    , signature =
                        Just
                            (Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } <|
                                { name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                                , typeAnnotation =
                                    Node { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } } <|
                                        Type (Node { end = { column = 10, row = 5 }, start = { column = 7, row = 5 } } ( [], "Int" )) []
                                }
                            )
                    , declaration =
                        Node { end = { column = 8, row = 6 }, start = { column = 1, row = 6 } }
                            { name = Node { start = { row = 6, column = 1 }, end = { row = 6, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 6, column = 7 }, end = { row = 6, column = 8 } } <| Integer 1
                            }
                    }
            ]
      , comments = []
      }
    )


functionWithSingleLineCommentAsDoc : ( String, String, File )
functionWithSingleLineCommentAsDoc =
    ( "functionWithSingleLineCommentAsDoc"
    , "\nmodule Bar exposing (..)\n\n"
        ++ "--The Doc\n"
        ++ "bar = 1\n"
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 8, row = 4 }, start = { column = 1, row = 4 } }
                            { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } <| Integer 1
                            }
                    }
            ]
      , comments = [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "--The Doc" ]
      }
    )


fileWithMultipleComments : ( String, String, File )
fileWithMultipleComments =
    ( "fileWithMultipleComments"
    , """
-- comment 1
module Bar exposing (..)

-- comment 2
bar = {- comment 3 -} 1 -- comment 4
 -- comment 5
"""
    , { moduleDefinition =
            Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 2 }, start = { column = 8, row = 2 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 2 }, start = { column = 12, row = 2 } } <| All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { end = { column = 24, row = 5 }, start = { column = 1, row = 5 } }
                (FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 24, row = 5 }, start = { column = 1, row = 5 } }
                            { arguments = []
                            , expression = Node { end = { column = 24, row = 5 }, start = { column = 23, row = 5 } } (Integer 1)
                            , name = Node { end = { column = 4, row = 5 }, start = { column = 1, row = 5 } } "bar"
                            }
                    }
                )
            ]
      , comments =
            [ Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } } "-- comment 1"
            , Node { end = { column = 13, row = 4 }, start = { column = 1, row = 4 } } "-- comment 2"
            , Node { end = { column = 22, row = 5 }, start = { column = 7, row = 5 } } "{- comment 3 -}"
            , Node { end = { column = 37, row = 5 }, start = { column = 25, row = 5 } } "-- comment 4"
            , Node { end = { column = 14, row = 6 }, start = { column = 2, row = 6 } } "-- comment 5"
            ]
      }
    )


functionWithMultiLineCommentAsDoc : ( String, String, File )
functionWithMultiLineCommentAsDoc =
    ( "functionWithMultiLineCommentAsDoc"
    , """
module Bar exposing (..)

{- The Doc -}
bar = 1
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList =
                        Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <|
                            All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 8, row = 4 }, start = { column = 1, row = 4 } }
                            { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } <| Integer 1
                            }
                    }
            ]
      , comments = [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } } "{- The Doc -}" ]
      }
    )


typeAliasWithDocumentation : ( String, String, File )
typeAliasWithDocumentation =
    ( "typeAliasWithDocumentation"
    , """
module Bar exposing (..)

{-| The Doc -}
type alias Foo
   = { name : String }
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 4, column = 1 }, end = { row = 5, column = 23 } } <|
                AliasDeclaration
                    { documentation =
                        Just <|
                            Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } } <|
                                "{-| The Doc -}"
                    , name = Node { end = { column = 15, row = 4 }, start = { column = 12, row = 4 } } "Foo"
                    , generics = []
                    , typeAnnotation =
                        Node { start = { row = 5, column = 6 }, end = { row = 5, column = 23 } } <|
                            Record
                                [ Node { end = { column = 21, row = 5 }, start = { column = 8, row = 5 } }
                                    ( Node { end = { column = 12, row = 5 }, start = { column = 8, row = 5 } } "name"
                                    , Node { start = { row = 5, column = 15 }, end = { row = 5, column = 21 } } <|
                                        Type (Node { end = { column = 21, row = 5 }, start = { column = 15, row = 5 } } ( [], "String" )) []
                                    )
                                ]
                    }
            ]
      , comments = []
      }
    )


typeWithDocumentation : ( String, String, File )
typeWithDocumentation =
    ( "typeWithDocumentation"
    , """
module Bar exposing (..)

{-| The Doc -}
type Foo
   = Red
   | Blue
"""
    , { comments = []
      , declarations =
            [ Node { end = { column = 10, row = 6 }, start = { column = 1, row = 4 } }
                (CustomTypeDeclaration
                    { firstConstructor =
                        Node { end = { column = 9, row = 5 }, start = { column = 6, row = 5 } }
                            { arguments = [], name = Node { end = { column = 9, row = 5 }, start = { column = 6, row = 5 } } "Red" }
                    , restOfConstructors =
                        [ Node { end = { column = 10, row = 6 }, start = { column = 6, row = 6 } }
                            { arguments = [], name = Node { end = { column = 10, row = 6 }, start = { column = 6, row = 6 } } "Blue" }
                        ]
                    , documentation =
                        Just
                            (Node { end = { column = 15, row = 3 }, start = { column = 1, row = 3 } }
                                "{-| The Doc -}"
                            )
                    , generics = []
                    , name = Node { end = { column = 9, row = 4 }, start = { column = 6, row = 4 } } "Foo"
                    }
                )
            ]
      , imports = []
      , moduleDefinition = Node { end = { column = 25, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } (All { end = { column = 24, row = 1 }, start = { column = 22, row = 1 } }), moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ] })
      }
    )


postProcessInfixOperators : ( String, String, File )
postProcessInfixOperators =
    ( "postProcessInfixOperators"
    , """
module Bar exposing (..)

bar = (x + 1) * (2 * y)
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 24, row = 3 }, start = { column = 1, row = 3 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } } <|
                                    OperatorApplication "*"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } } <|
                                            TupleExpression
                                                [ Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } } <|
                                                    OperatorApplication "+"
                                                        Left
                                                        (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } <|
                                                            FunctionOrValue [] "x"
                                                        )
                                                        (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <| Integer 1)
                                                ]
                                        )
                                        (Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } } <|
                                            TupleExpression
                                                [ Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } } <|
                                                    OperatorApplication "*"
                                                        Left
                                                        (Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } <| Integer 2)
                                                        (Node { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } } <| FunctionOrValue [] "y")
                                                ]
                                        )
                            }
                    }
            ]
      , comments = []
      }
    )


postProcessInfixOperators2 : ( String, String, File )
postProcessInfixOperators2 =
    ( "postProcessInfixOperators2"
    , """
module Bar exposing (..)

bar = x + 1 * 2
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 16, row = 3 }, start = { column = 1, row = 3 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } } <|
                                    OperatorApplication "+"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } <|
                                            FunctionOrValue [] "x"
                                        )
                                        (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } } <|
                                            OperatorApplication "*"
                                                Left
                                                (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } <| Integer 1)
                                                (Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } <| Integer 2)
                                        )
                            }
                    }
            ]
      , comments = []
      }
    )


postProcessInfixOperators3 : ( String, String, File )
postProcessInfixOperators3 =
    ( "postProcessInfixOperators3"
    , """
module Bar exposing (..)

bar = x * 1 + 2
"""
    , { moduleDefinition =
            Node
                { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
            <|
                NormalModule
                    { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } [ "Bar" ]
                    , exposingList = Node { end = { column = 25, row = 1 }, start = { column = 12, row = 1 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { end = { column = 16, row = 3 }, start = { column = 1, row = 3 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } } <|
                                    OperatorApplication "+"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } } <|
                                            OperatorApplication "*"
                                                Left
                                                (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } <| FunctionOrValue [] "x")
                                                (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } <| Integer 1)
                                        )
                                        (Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } <| Integer 2)
                            }
                    }
            ]
      , comments = []
      }
    )


portWithDocumentation : ( String, String, File )
portWithDocumentation =
    ( "portWithDocumentation"
    , """module A exposing (..)

{-| Some port documentation -}
port foo : String -> Cmd msg"""
    , { comments = []
      , declarations =
            [ Node { end = { column = 1, row = 5 }, start = { column = 1, row = 3 } }
                (PortDeclaration
                    { documentation = Just (Node { end = { column = 31, row = 3 }, start = { column = 1, row = 3 } } "{-| Some port documentation -}")
                    , signature =
                        Node { end = { column = 1, row = 5 }, start = { column = 6, row = 4 } }
                            { name = Node { end = { column = 9, row = 4 }, start = { column = 6, row = 4 } } "foo"
                            , typeAnnotation =
                                Node { end = { column = 29, row = 4 }, start = { column = 12, row = 4 } }
                                    (FunctionTypeAnnotation
                                        (Node { end = { column = 18, row = 4 }, start = { column = 12, row = 4 } }
                                            (Type (Node { end = { column = 18, row = 4 }, start = { column = 12, row = 4 } } ( [], "String" )) [])
                                        )
                                        (Node { end = { column = 29, row = 4 }, start = { column = 22, row = 4 } }
                                            (Type (Node { end = { column = 25, row = 4 }, start = { column = 22, row = 4 } } ( [], "Cmd" ))
                                                [ Node { end = { column = 29, row = 4 }, start = { column = 26, row = 4 } } (Var "msg") ]
                                            )
                                        )
                                    )
                            }
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                (NormalModule
                    { exposingList =
                        Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                            (All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                    , moduleName = Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                    }
                )
      }
    )


{-| Check to make sure this issue is fixed <https://github.com/stil4m/elm-syntax/issues/41>
-}
postProcessInfixOperatorsRegressionTest : ( String, String, File )
postProcessInfixOperatorsRegressionTest =
    ( "postProcessInfixOperatorsRegressionTest"
    , """
module A exposing (..)

bool1 = True && True || True
bool2 = True || True && True

numeric1 = 1 ^ 2 * 3 + 4
numeric2 = 1 + 2 * 3 ^ 4
"""
    , { comments = []
      , declarations =
            [ Node { end = { column = 29, row = 3 }, start = { column = 1, row = 3 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 29, row = 3 }, start = { column = 1, row = 3 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 29, row = 3 }, start = { column = 9, row = 3 } }
                                    (OperatorApplication "||"
                                        Right
                                        (Node { end = { column = 21, row = 3 }, start = { column = 9, row = 3 } }
                                            (OperatorApplication "&&"
                                                Right
                                                (Node { end = { column = 13, row = 3 }, start = { column = 9, row = 3 } } (FunctionOrValue [] "True"))
                                                (Node { end = { column = 21, row = 3 }, start = { column = 17, row = 3 } } (FunctionOrValue [] "True"))
                                            )
                                        )
                                        (Node { end = { column = 29, row = 3 }, start = { column = 25, row = 3 } } (FunctionOrValue [] "True"))
                                    )
                            , name = Node { end = { column = 6, row = 3 }, start = { column = 1, row = 3 } } "bool1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { end = { column = 29, row = 4 }, start = { column = 1, row = 4 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 29, row = 4 }, start = { column = 1, row = 4 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 29, row = 4 }, start = { column = 9, row = 4 } }
                                    (OperatorApplication "||"
                                        Right
                                        (Node { end = { column = 13, row = 4 }, start = { column = 9, row = 4 } } (FunctionOrValue [] "True"))
                                        (Node { end = { column = 29, row = 4 }, start = { column = 17, row = 4 } }
                                            (OperatorApplication "&&"
                                                Right
                                                (Node { end = { column = 21, row = 4 }, start = { column = 17, row = 4 } } (FunctionOrValue [] "True"))
                                                (Node { end = { column = 29, row = 4 }, start = { column = 25, row = 4 } } (FunctionOrValue [] "True"))
                                            )
                                        )
                                    )
                            , name = Node { end = { column = 6, row = 4 }, start = { column = 1, row = 4 } } "bool2"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { end = { column = 25, row = 6 }, start = { column = 1, row = 6 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 25, row = 6 }, start = { column = 1, row = 6 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 25, row = 6 }, start = { column = 12, row = 6 } }
                                    (OperatorApplication "+"
                                        Left
                                        (Node { end = { column = 21, row = 6 }, start = { column = 12, row = 6 } }
                                            (OperatorApplication "*"
                                                Left
                                                (Node { end = { column = 17, row = 6 }, start = { column = 12, row = 6 } }
                                                    (OperatorApplication "^"
                                                        Right
                                                        (Node { end = { column = 13, row = 6 }, start = { column = 12, row = 6 } } (Integer 1))
                                                        (Node { end = { column = 17, row = 6 }, start = { column = 16, row = 6 } } (Integer 2))
                                                    )
                                                )
                                                (Node { end = { column = 21, row = 6 }, start = { column = 20, row = 6 } } (Integer 3))
                                            )
                                        )
                                        (Node
                                            { end = { column = 25, row = 6 }, start = { column = 24, row = 6 } }
                                            (Integer 4)
                                        )
                                    )
                            , name = Node { end = { column = 9, row = 6 }, start = { column = 1, row = 6 } } "numeric1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { end = { column = 25, row = 7 }, start = { column = 1, row = 7 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 25, row = 7 }, start = { column = 1, row = 7 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 25, row = 7 }, start = { column = 12, row = 7 } }
                                    (OperatorApplication "+"
                                        Left
                                        (Node { end = { column = 13, row = 7 }, start = { column = 12, row = 7 } } (Integer 1))
                                        (Node { end = { column = 25, row = 7 }, start = { column = 16, row = 7 } }
                                            (OperatorApplication "*"
                                                Left
                                                (Node { end = { column = 17, row = 7 }, start = { column = 16, row = 7 } } (Integer 2))
                                                (Node { end = { column = 25, row = 7 }, start = { column = 20, row = 7 } }
                                                    (OperatorApplication "^"
                                                        Right
                                                        (Node { end = { column = 21, row = 7 }, start = { column = 20, row = 7 } } (Integer 3))
                                                        (Node { end = { column = 25, row = 7 }, start = { column = 24, row = 7 } } (Integer 4))
                                                    )
                                                )
                                            )
                                        )
                                    )
                            , name = Node { end = { column = 9, row = 7 }, start = { column = 1, row = 7 } } "numeric2"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                (NormalModule
                    { exposingList =
                        Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                            (All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                    , moduleName = Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                    }
                )
      }
    )


{-| Check to make sure this issue is fixed <https://github.com/stil4m/elm-syntax/issues/87>
-}
postProcessInfixOperatorsAssociativityTest : ( String, String, File )
postProcessInfixOperatorsAssociativityTest =
    ( "postProcessInfixOperatorsAssociativityTest"
    , """
module A exposing (..)

numeric1 = 1 + 2 - 3

pipeline1 = 1 |> 2 |> 3
"""
    , { comments = []
      , declarations =
            [ Node { end = { column = 21, row = 3 }, start = { column = 1, row = 3 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 21, row = 3 }, start = { column = 1, row = 3 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 21, row = 3 }, start = { column = 12, row = 3 } }
                                    (OperatorApplication "-"
                                        Left
                                        (Node { end = { column = 17, row = 3 }, start = { column = 12, row = 3 } }
                                            (OperatorApplication "+"
                                                Left
                                                (Node { end = { column = 13, row = 3 }, start = { column = 12, row = 3 } } (Integer 1))
                                                (Node { end = { column = 17, row = 3 }, start = { column = 16, row = 3 } } (Integer 2))
                                            )
                                        )
                                        (Node { end = { column = 21, row = 3 }, start = { column = 20, row = 3 } } (Integer 3))
                                    )
                            , name = Node { end = { column = 9, row = 3 }, start = { column = 1, row = 3 } } "numeric1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { end = { column = 24, row = 5 }, start = { column = 1, row = 5 } }
                (FunctionDeclaration
                    { declaration =
                        Node { end = { column = 24, row = 5 }, start = { column = 1, row = 5 } }
                            { arguments = []
                            , expression =
                                Node { end = { column = 24, row = 5 }, start = { column = 13, row = 5 } }
                                    (OperatorApplication "|>"
                                        Left
                                        (Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                            (OperatorApplication "|>"
                                                Left
                                                (Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } } (Integer 1))
                                                (Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } } (Integer 2))
                                            )
                                        )
                                        (Node { end = { column = 24, row = 5 }, start = { column = 23, row = 5 } } (Integer 3))
                                    )
                            , name = Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                (NormalModule
                    { exposingList =
                        Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                            (All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                    , moduleName = Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                    }
                )
      }
    )


suite : Test
suite =
    describe "Elm.Processing"
        (List.map
            (\( name, input, output ) ->
                test name <|
                    \() ->
                        case context of
                            Ok context_ ->
                                Parser.parse (String.trim input)
                                    |> Result.map (Processing.process context_)
                                    |> Expect.equal (Ok output)

                            Err _ ->
                                Expect.fail "Failed to generate context."
            )
            [ functionWithDocs
            , functionWithDocsAndSignature
            , functionWithSingleLineCommentAsDoc
            , fileWithMultipleComments
            , functionWithMultiLineCommentAsDoc
            , postProcessInfixOperators
            , postProcessInfixOperators2
            , postProcessInfixOperators3
            , postProcessInfixOperatorsRegressionTest
            , postProcessInfixOperatorsAssociativityTest
            , typeAliasWithDocumentation
            , portWithDocumentation
            , typeWithDocumentation
            ]
        )


context : Result (List DeadEnd) Processing.ProcessContext
context =
    """
module Basics exposing ((+), (-), (*), (/), (//), (^), (==), (/=), (<), (>), (<=), (>=), (&&), (||), (++), (<|), (|>), (<<), (>>))


infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<)  = lt
infix non   4 (>)  = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+)  = add
infix left  6 (-)  = sub
infix left  7 (*)  = mul
infix left  7 (/)  = fdiv
infix left  7 (//) = idiv
infix right 8 (^)  = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR"""
        |> String.trim
        |> Parser.parse
        |> Result.map (\a -> Processing.addFile a Processing.init)
