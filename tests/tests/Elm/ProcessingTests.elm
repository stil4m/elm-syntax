module Elm.ProcessingTests exposing (functionWithDocs, functionWithDocsAndSignature, functionWithMultiLineCommentAsDoc, functionWithSingleLineCommentAsDoc, postProcessInfixOperators, suite, typeAliasWithDocumentation)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


functionWithDocs : ( String, String, File )
functionWithDocs =
    ( "functionWithDocs"
    , """
module Bar exposing (..)

{-| The docs
-}
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
    , """
module Bar exposing (..)

{-| The docs
-}
bar : Int
bar = 1
"""
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
                                        Typed (Node { end = { column = 10, row = 5 }, start = { column = 7, row = 5 } } ( [], "Int" )) []
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
    , """
module Bar exposing (..)

--The Doc
bar = 1
"""
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
                                        Typed (Node { end = { column = 21, row = 5 }, start = { column = 15, row = 5 } } ( [], "String" )) []
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
                    { constructors =
                        [ Node { end = { column = 9, row = 5 }, start = { column = 6, row = 5 } }
                            { arguments = [], name = Node { end = { column = 9, row = 5 }, start = { column = 6, row = 5 } } "Red" }
                        , Node { end = { column = 10, row = 6 }, start = { column = 6, row = 6 } }
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
                                            ParenthesizedExpression
                                                (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } } <|
                                                    OperatorApplication "+"
                                                        Left
                                                        (Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } <|
                                                            FunctionOrValue [] "x"
                                                        )
                                                        (Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <| Integer 1)
                                                )
                                        )
                                        (Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } } <|
                                            ParenthesizedExpression
                                                (Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } } <|
                                                    OperatorApplication "*"
                                                        Left
                                                        (Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } <| Integer 2)
                                                        (Node { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } } <| FunctionOrValue [] "y")
                                                )
                                        )
                            }
                    }
            ]
      , comments = []
      }
    )


suite : Test
suite =
    describe "Elm.Processing"
        (List.map
            (\( name, input, output ) ->
                test name <|
                    \() ->
                        Parser.parse (String.trim input)
                            |> Result.map (Processing.process Processing.init)
                            |> Expect.equal (Ok output)
            )
            [ functionWithDocs
            , functionWithDocsAndSignature
            , functionWithSingleLineCommentAsDoc
            , functionWithMultiLineCommentAsDoc
            , postProcessInfixOperators
            , typeAliasWithDocumentation
            , typeWithDocumentation
            ]
        )
