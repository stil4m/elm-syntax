module Elm.ProcessingTests exposing (suite)

import Elm.Parser as Parser
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Elm.Processing"
        (List.map
            (\( name, input, output ) ->
                test name <|
                    \() ->
                        Parser.parse (String.trim input)
                            |> Expect.equal (Ok output)
            )
            testCases
        )


testCases : List ( String, String, File )
testCases =
    [ functionWithDocs
    , functionWithDocsAndSignature
    , functionWithSingleLineCommentAsDoc
    , fileWithMultipleComments
    , functionWithMultiLineCommentAsDoc
    , postProcessInfixOperators
    , postProcessInfixOperators2
    , postProcessInfixOperators3
    , postProcessInfixOperatorsInNegation
    , postProcessInfixOperatorsInRecordAccess
    , postProcessInfixOperatorsRegressionTest
    , postProcessInfixOperatorsAssociativityTest
    , typeAliasWithDocumentation
    , typeWithDocumentation
    , portWithDocumentation
    , maxCallStackSizeFailure
    ]


functionWithDocs : ( String, String, File )
functionWithDocs =
    ( "functionWithDocs"
    , """
module Bar exposing (..)

{-| The docs
-}
bar = 1
""" |> String.replace "\u{000D}" ""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList =
                        Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <|
                            All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 5, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Just (Node { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } } "{-| The docs\n-}")
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                            { name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 5, column = 7 }, end = { row = 5, column = 8 } } <| IntegerLiteral 1
                            }
                    }
            ]
      , comments = []
      }
    )


functionWithDocsAndSignature : ( String, String, File )
functionWithDocsAndSignature =
    ( "functionWithDocsAndSignature"
    , """
module Bar exposing (..)

{-| The docs
-}
bar : Int
bar = 1
""" |> String.replace "\u{000D}" ""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 6, column = 8 } } <|
                FunctionDeclaration
                    { documentation =
                        Just <| Node { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } } "{-| The docs\n-}"
                    , signature =
                        Just
                            (Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } <|
                                { name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                                , typeAnnotation =
                                    Node { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } } <|
                                        Type (Node { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } } ( [], "Int" )) []
                                }
                            )
                    , declaration =
                        Node { start = { row = 6, column = 1 }, end = { row = 6, column = 8 } }
                            { name = Node { start = { row = 6, column = 1 }, end = { row = 6, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 6, column = 7 }, end = { row = 6, column = 8 } } <| IntegerLiteral 1
                            }
                    }
            ]
      , comments = []
      }
    )


functionWithSingleLineCommentAsDoc : ( String, String, File )
functionWithSingleLineCommentAsDoc =
    ( "functionWithSingleLineCommentAsDoc"
    , """
module Bar exposing (..)

--The Doc
bar = 1
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                            { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } <| IntegerLiteral 1
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
                    { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } } <| All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                (FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                            { arguments = []
                            , expression = Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (IntegerLiteral 1)
                            , name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                            }
                    }
                )
            ]
      , comments =
            [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } "-- comment 1"
            , Node { start = { row = 4, column = 1 }, end = { row = 4, column = 13 } } "-- comment 2"
            , Node { start = { row = 5, column = 7 }, end = { row = 5, column = 22 } } "{- comment 3 -}"
            , Node { start = { row = 5, column = 25 }, end = { row = 5, column = 37 } } "-- comment 4"
            , Node { start = { row = 6, column = 2 }, end = { row = 6, column = 14 } } "-- comment 5"
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
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList =
                        Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <|
                            All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                            { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                            , arguments = []
                            , expression = Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } <| IntegerLiteral 1
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
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 5, column = 23 } } <|
                AliasDeclaration
                    { documentation =
                        Just <|
                            Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } } "{-| The Doc -}"
                    , name = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 15 } } "Foo"
                    , generics = []
                    , typeAnnotation =
                        Node { start = { row = 5, column = 6 }, end = { row = 5, column = 23 } } <|
                            Record
                                [ Node { start = { row = 5, column = 8 }, end = { row = 5, column = 21 } }
                                    ( Node { start = { row = 5, column = 8 }, end = { row = 5, column = 12 } } "name"
                                    , Node { start = { row = 5, column = 15 }, end = { row = 5, column = 21 } } <|
                                        Type (Node { start = { row = 5, column = 15 }, end = { row = 5, column = 21 } } ( [], "String" )) []
                                    )
                                ]
                    }
            ]
      , comments = []
      }
    )


maxCallStackSizeFailure : ( String, String, File )
maxCallStackSizeFailure =
    ( "maxCallStackSizeFailure"
    , """module Simplify.AstHelpers exposing (log)


log : Int -> Int
log a =
    Debug.log "ok" a
"""
    , { comments = []
      , declarations =
            [ Node { end = { column = 21, row = 6 }, start = { column = 1, row = 4 } }
                (FunctionDeclaration
                    { declaration = Node { end = { column = 21, row = 6 }, start = { column = 1, row = 5 } } { arguments = [ Node { end = { column = 6, row = 5 }, start = { column = 5, row = 5 } } (VarPattern_ "a") ], expression = Node { end = { column = 21, row = 6 }, start = { column = 5, row = 6 } } (Application (Node { end = { column = 14, row = 6 }, start = { column = 5, row = 6 } } (FunctionOrValue [ "Debug" ] "log")) [ Node { end = { column = 19, row = 6 }, start = { column = 15, row = 6 } } (StringLiteral SingleQuote "ok"), Node { end = { column = 21, row = 6 }, start = { column = 20, row = 6 } } (FunctionOrValue [] "a") ]), name = Node { end = { column = 4, row = 5 }, start = { column = 1, row = 5 } } "log" }
                    , documentation = Nothing
                    , signature = Just (Node { end = { column = 17, row = 4 }, start = { column = 1, row = 4 } } { name = Node { end = { column = 4, row = 4 }, start = { column = 1, row = 4 } } "log", typeAnnotation = Node { end = { column = 17, row = 4 }, start = { column = 7, row = 4 } } (FunctionTypeAnnotation (Node { end = { column = 10, row = 4 }, start = { column = 7, row = 4 } } (Type (Node { end = { column = 10, row = 4 }, start = { column = 7, row = 4 } } ( [], "Int" )) [])) (Node { end = { column = 17, row = 4 }, start = { column = 14, row = 4 } } (Type (Node { end = { column = 17, row = 4 }, start = { column = 14, row = 4 } } ( [], "Int" )) []))) })
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { end = { column = 42, row = 1 }, start = { column = 1, row = 1 } }
                (NormalModule
                    { exposingList =
                        Node { end = { column = 42, row = 1 }, start = { column = 28, row = 1 } }
                            (Explicit
                                (Node { end = { column = 41, row = 1 }, start = { column = 38, row = 1 } } (FunctionExpose "log"))
                                []
                            )
                    , moduleName = Node { end = { column = 27, row = 1 }, start = { column = 8, row = 1 } } [ "Simplify", "AstHelpers" ]
                    }
                )
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
            [ Node { start = { row = 3, column = 1 }, end = { row = 6, column = 10 } }
                (CustomTypeDeclaration
                    { firstConstructor =
                        Node { start = { row = 5, column = 6 }, end = { row = 5, column = 9 } }
                            { arguments = [], name = Node { start = { row = 5, column = 6 }, end = { row = 5, column = 9 } } "Red" }
                    , restOfConstructors =
                        [ Node { start = { row = 6, column = 6 }, end = { row = 6, column = 10 } }
                            { arguments = [], name = Node { start = { row = 6, column = 6 }, end = { row = 6, column = 10 } } "Blue" }
                        ]
                    , documentation =
                        Just
                            (Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                "{-| The Doc -}"
                            )
                    , generics = []
                    , name = Node { start = { row = 4, column = 6 }, end = { row = 4, column = 9 } } "Foo"
                    }
                )
            ]
      , imports = []
      , moduleDefinition = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } (NormalModule { exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }), moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ] })
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
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } } <|
                                    Operation "*"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } } <|
                                            TupleExpression
                                                [ Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } } <|
                                                    Operation "+"
                                                        Left
                                                        (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } <|
                                                            FunctionOrValue [] "x"
                                                        )
                                                        (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <| IntegerLiteral 1)
                                                ]
                                        )
                                        (Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } } <|
                                            TupleExpression
                                                [ Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } } <|
                                                    Operation "*"
                                                        Left
                                                        (Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } <| IntegerLiteral 2)
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
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } } <|
                                    Operation "+"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } <|
                                            FunctionOrValue [] "x"
                                        )
                                        (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } } <|
                                            Operation "*"
                                                Left
                                                (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } <| IntegerLiteral 1)
                                                (Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } <| IntegerLiteral 2)
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
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } } <|
                FunctionDeclaration
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                            { name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            , arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } } <|
                                    Operation "+"
                                        Left
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } } <|
                                            Operation "*"
                                                Left
                                                (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } <| FunctionOrValue [] "x")
                                                (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } <| IntegerLiteral 1)
                                        )
                                        (Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } <| IntegerLiteral 2)
                            }
                    }
            ]
      , comments = []
      }
    )


postProcessInfixOperatorsInNegation : ( String, String, File )
postProcessInfixOperatorsInNegation =
    ( "postProcessInfixOperatorsInNegation"
    , """
module Bar exposing (..)

bar = -(1 * 2)
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 15 } }
                                    (Negation
                                        (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 15 } }
                                            (TupleExpression
                                                [ Node { start = { row = 3, column = 9 }, end = { row = 3, column = 14 } }
                                                    (Operation
                                                        "*"
                                                        Left
                                                        (Node { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } } (IntegerLiteral 1))
                                                        (Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (IntegerLiteral 2))
                                                    )
                                                ]
                                            )
                                        )
                                    )
                            , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            ]
      , comments = []
      }
    )


postProcessInfixOperatorsInRecordAccess : ( String, String, File )
postProcessInfixOperatorsInRecordAccess =
    ( "postProcessInfixOperatorsInRecordAccess"
    , """
module Bar exposing (..)

bar = (1 * 2).x
"""
    , { moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } <|
                NormalModule
                    { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                    , exposingList = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } <| All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                    }
      , imports = []
      , declarations =
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                    (RecordAccess
                                        (Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                            (TupleExpression
                                                [ Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                    (Operation
                                                        "*"
                                                        Left
                                                        (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (IntegerLiteral 1))
                                                        (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (IntegerLiteral 2))
                                                    )
                                                ]
                                            )
                                        )
                                        (Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } "x")
                                    )
                            , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
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
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 3, column = 9 }, end = { row = 3, column = 29 } }
                                    (Operation "||"
                                        Right
                                        (Node { start = { row = 3, column = 9 }, end = { row = 3, column = 21 } }
                                            (Operation "&&"
                                                Right
                                                (Node { start = { row = 3, column = 9 }, end = { row = 3, column = 13 } } (FunctionOrValue [] "True"))
                                                (Node { start = { row = 3, column = 17 }, end = { row = 3, column = 21 } } (FunctionOrValue [] "True"))
                                            )
                                        )
                                        (Node { start = { row = 3, column = 25 }, end = { row = 3, column = 29 } } (FunctionOrValue [] "True"))
                                    )
                            , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } } "bool1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 4, column = 9 }, end = { row = 4, column = 29 } }
                                    (Operation "||"
                                        Right
                                        (Node { start = { row = 4, column = 9 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "True"))
                                        (Node { start = { row = 4, column = 17 }, end = { row = 4, column = 29 } }
                                            (Operation "&&"
                                                Right
                                                (Node { start = { row = 4, column = 17 }, end = { row = 4, column = 21 } } (FunctionOrValue [] "True"))
                                                (Node { start = { row = 4, column = 25 }, end = { row = 4, column = 29 } } (FunctionOrValue [] "True"))
                                            )
                                        )
                                    )
                            , name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 6 } } "bool2"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 6, column = 12 }, end = { row = 6, column = 25 } }
                                    (Operation "+"
                                        Left
                                        (Node { start = { row = 6, column = 12 }, end = { row = 6, column = 21 } }
                                            (Operation "*"
                                                Left
                                                (Node { start = { row = 6, column = 12 }, end = { row = 6, column = 17 } }
                                                    (Operation "^"
                                                        Right
                                                        (Node { start = { row = 6, column = 12 }, end = { row = 6, column = 13 } } (IntegerLiteral 1))
                                                        (Node { start = { row = 6, column = 16 }, end = { row = 6, column = 17 } } (IntegerLiteral 2))
                                                    )
                                                )
                                                (Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (IntegerLiteral 3))
                                            )
                                        )
                                        (Node
                                            { start = { row = 6, column = 24 }, end = { row = 6, column = 25 } }
                                            (IntegerLiteral 4)
                                        )
                                    )
                            , name = Node { start = { row = 6, column = 1 }, end = { row = 6, column = 9 } } "numeric1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 7, column = 12 }, end = { row = 7, column = 25 } }
                                    (Operation "+"
                                        Left
                                        (Node { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } } (IntegerLiteral 1))
                                        (Node { start = { row = 7, column = 16 }, end = { row = 7, column = 25 } }
                                            (Operation "*"
                                                Left
                                                (Node { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } } (IntegerLiteral 2))
                                                (Node { start = { row = 7, column = 20 }, end = { row = 7, column = 25 } }
                                                    (Operation "^"
                                                        Right
                                                        (Node { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } } (IntegerLiteral 3))
                                                        (Node { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } } (IntegerLiteral 4))
                                                    )
                                                )
                                            )
                                        )
                                    )
                            , name = Node { start = { row = 7, column = 1 }, end = { row = 7, column = 9 } } "numeric2"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                (NormalModule
                    { exposingList =
                        Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                            (All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                    , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
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
            [ Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 3, column = 12 }, end = { row = 3, column = 21 } }
                                    (Operation "-"
                                        Left
                                        (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 17 } }
                                            (Operation "+"
                                                Left
                                                (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (IntegerLiteral 1))
                                                (Node { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } } (IntegerLiteral 2))
                                            )
                                        )
                                        (Node { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } } (IntegerLiteral 3))
                                    )
                            , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } } "numeric1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            , Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                (FunctionDeclaration
                    { declaration =
                        Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                            { arguments = []
                            , expression =
                                Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                    (Operation "|>"
                                        Left
                                        (Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                            (Operation "|>"
                                                Left
                                                (Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (IntegerLiteral 1))
                                                (Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (IntegerLiteral 2))
                                            )
                                        )
                                        (Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (IntegerLiteral 3))
                                    )
                            , name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                            }
                    , documentation = Nothing
                    , signature = Nothing
                    }
                )
            ]
      , imports = []
      , moduleDefinition =
            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                (NormalModule
                    { exposingList =
                        Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                            (All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                    , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                    }
                )
      }
    )
