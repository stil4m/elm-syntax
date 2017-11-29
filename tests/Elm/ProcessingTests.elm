module Elm.ProcessingTests exposing (..)

import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
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
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } }
                }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just (Documentation "{-| The docs\n-}" { start = { row = 3, column = 0 }, end = { row = 4, column = 2 } })
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 5, column = 0 }, end = { row = 5, column = 3 } } }
                    , arguments = []
                    , expression = ( { start = { row = 5, column = 6 }, end = { row = 5, column = 7 } }, Integer 1 )
                    }
                }
            ]
      , comments = []
      }
    )


operatorFunctionWithDocs : ( String, String, File )
operatorFunctionWithDocs =
    ( "operatorFunctionWithDocs"
    , """
module Bar exposing (..)

{-| The docs
-}
(++) = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } } }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just { text = "{-| The docs\n-}", range = { start = { row = 3, column = 0 }, end = { row = 4, column = 2 } } }
                , signature = Nothing
                , declaration =
                    { operatorDefinition = True
                    , name = { value = "++", range = { start = { row = 5, column = 0 }, end = { row = 5, column = 4 } } }
                    , arguments = []
                    , expression = ( { start = { row = 5, column = 7 }, end = { row = 5, column = 8 } }, Integer 1 )
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
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } } }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just { text = "{-| The docs\n-}", range = { start = { row = 3, column = 0 }, end = { row = 4, column = 2 } } }
                , signature =
                    Just
                        { operatorDefinition = False
                        , name = "bar"
                        , typeAnnotation = ( { start = { row = 5, column = 6 }, end = { row = 5, column = 9 } }, Typed [] "Int" [] )
                        , range = { start = { row = 5, column = 0 }, end = { row = 5, column = 9 } }
                        }
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 6, column = 0 }, end = { row = 6, column = 3 } } }
                    , arguments = []
                    , expression = ( { start = { row = 6, column = 6 }, end = { row = 6, column = 7 } }, Integer 1 )
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
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } } }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 4, column = 0 }, end = { row = 4, column = 3 } } }
                    , arguments = []
                    , expression = ( { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } }, Integer 1 )
                    }
                }
            ]
      , comments = [ ( { start = { row = 3, column = 0 }, end = { row = 3, column = 9 } }, "--The Doc" ) ]
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
            NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } } }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 4, column = 0 }, end = { row = 4, column = 3 } } }
                    , arguments = []
                    , expression = ( { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } }, Integer 1 )
                    }
                }
            ]
      , comments = [ ( { start = { row = 3, column = 0 }, end = { row = 3, column = 13 } }, "{- The Doc -}" ) ]
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
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } }
                }
      , imports = []
      , declarations =
            [ AliasDecl
                { documentation = Just { text = "{-| The Doc -}", range = { start = { row = 3, column = 0 }, end = { row = 3, column = 14 } } }
                , name = "Foo"
                , generics = []
                , typeAnnotation =
                    ( { start = { row = 5, column = 5 }, end = { row = 5, column = 22 } }
                    , Record [ ( "name", ( { start = { row = 5, column = 14 }, end = { row = 5, column = 21 } }, Typed [] "String" [] ) ) ]
                    )
                , range = { start = { row = 4, column = 0 }, end = { row = 5, column = 22 } }
                }
            ]
      , comments = []
      }
    )


postProcessInfixOperators : ( String, String, File )
postProcessInfixOperators =
    ( "postProcessInfixOperators"
    , """
module Bar exposing (..)

bar = (x + 1) * (2 * y)
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 0, column = 20 }, end = { row = 0, column = 22 } } }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 3, column = 0 }, end = { row = 3, column = 3 } } }
                    , arguments = []
                    , expression =
                        ( { start = { row = 3, column = 6 }, end = { row = 3, column = 23 } }
                        , OperatorApplication "*"
                            Left
                            ( { start = { row = 3, column = 6 }, end = { row = 3, column = 13 } }
                            , ParenthesizedExpression ( { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }, OperatorApplication "+" Left ( { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } }, FunctionOrValue "x" ) ( { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }, Integer 1 ) )
                            )
                            ( { start = { row = 3, column = 16 }, end = { row = 3, column = 23 } }, ParenthesizedExpression ( { start = { row = 3, column = 17 }, end = { row = 3, column = 22 } }, OperatorApplication "*" Left ( { start = { row = 3, column = 17 }, end = { row = 3, column = 18 } }, Integer 2 ) ( { start = { row = 3, column = 21 }, end = { row = 3, column = 22 } }, FunctionOrValue "y" ) ) )
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
                        Parser.parse input
                            |> Result.map (Processing.process Processing.init)
                            |> Expect.equal (Ok output)
            )
            [ functionWithDocs
            , operatorFunctionWithDocs
            , functionWithDocsAndSignature
            , functionWithSingleLineCommentAsDoc
            , functionWithMultiLineCommentAsDoc
            , postProcessInfixOperators
            , typeAliasWithDocumentation
            ]
        )
