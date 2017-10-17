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
module Bar

{-| The docs
-}
bar = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just (Documentation "{-| The docs\n-}" { start = { row = 3, column = -1 }, end = { row = 5, column = -2 } })
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 5, column = -1 }, end = { row = 5, column = 2 } } }
                    , arguments = []
                    , expression = ( { start = { row = 5, column = 5 }, end = { row = 6, column = -2 } }, Integer 1 )
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
module Bar

{-| The docs
-}
(++) = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just (Documentation "{-| The docs\n-}" { start = { row = 3, column = -1 }, end = { row = 5, column = -2 } })
                , signature = Nothing
                , declaration =
                    { operatorDefinition = True
                    , name = { value = "++", range = { start = { row = 5, column = -1 }, end = { row = 5, column = 3 } } }
                    , arguments = []
                    , expression = ( { start = { row = 5, column = 6 }, end = { row = 6, column = -2 } }, Integer 1 )
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
module Bar

{-| The docs
-}
bar : Int
bar = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Just (Documentation "{-| The docs\n-}" { start = { row = 3, column = -1 }, end = { row = 5, column = -2 } })
                , signature = Just { operatorDefinition = False, name = "bar", typeAnnotation = Typed [] "Int" [] { start = { row = 5, column = 5 }, end = { row = 6, column = -2 } }, range = { start = { row = 5, column = -1 }, end = { row = 6, column = -2 } } }
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 6, column = -1 }, end = { row = 6, column = 2 } } }
                    , arguments = []
                    , expression = ( { start = { row = 6, column = 5 }, end = { row = 7, column = -2 } }, Integer 1 )
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
module Bar

--The Doc
bar = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 4, column = -1 }, end = { row = 4, column = 2 } } }
                    , arguments = []
                    , expression = ( { start = { row = 4, column = 5 }, end = { row = 5, column = -2 } }, Integer 1 )
                    }
                }
            ]
      , comments = [ ( "--The Doc", { start = { row = 3, column = -1 }, end = { row = 4, column = -2 } } ) ]
      }
    )


functionWithMultiLineCommentAsDoc : ( String, String, File )
functionWithMultiLineCommentAsDoc =
    ( "functionWithMultiLineCommentAsDoc"
    , """
module Bar

{- The Doc -}
bar = 1
"""
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 4, column = -1 }, end = { row = 4, column = 2 } } }
                    , arguments = []
                    , expression = ( { start = { row = 4, column = 5 }, end = { row = 5, column = -2 } }, Integer 1 )
                    }
                }
            ]
      , comments = [ ( "{- The Doc -}", { start = { row = 3, column = -1 }, end = { row = 4, column = -2 } } ) ]
      }
    )


typeAliasWithDocumentation : ( String, String, File )
typeAliasWithDocumentation =
    ( "typeAliasWithDocumentation"
    , """
module Bar

{-| The Doc -}
type alias Foo
   = { name : String }
"""
    , { moduleDefinition =
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = None
                }
      , imports = []
      , declarations =
            [ AliasDecl
                { documentation = Just (Documentation "{-| The Doc -}" { start = { row = 3, column = -1 }, end = { row = 4, column = -2 } })
                , name = "Foo"
                , generics = []
                , typeAnnotation = Record [ ( "name", Typed [] "String" [] { start = { row = 5, column = 13 }, end = { row = 5, column = 20 } } ) ] { start = { row = 5, column = 4 }, end = { row = 6, column = -2 } }
                , range = { start = { row = 4, column = -1 }, end = { row = 6, column = -2 } }
                }
            ]
      , comments = []
      }
    )


postProcessInfixOperators : ( String, String, File )
postProcessInfixOperators =
    ( "postProcessInfixOperators"
    , """
module Bar

bar = (x + 1) * (2 * y)
"""
    , { moduleDefinition =
            NormalModule { moduleName = [ "Bar" ], exposingList = None }
      , imports = []
      , declarations =
            [ FuncDecl
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    { operatorDefinition = False
                    , name = { value = "bar", range = { start = { row = 3, column = -1 }, end = { row = 3, column = 2 } } }
                    , arguments = []
                    , expression =
                        ( { start = { row = 3, column = 5 }, end = { row = 4, column = -2 } }
                        , OperatorApplication "*"
                            Left
                            ( { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }, ParenthesizedExpression ( { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }, OperatorApplication "+" Left ( { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } }, FunctionOrValue "x" ) ( { start = { row = 3, column = 10 }, end = { row = 3, column = 11 } }, Integer 1 ) ) )
                            ( { start = { row = 3, column = 15 }, end = { row = 4, column = -2 } }
                            , ParenthesizedExpression
                                ( { start = { row = 3, column = 16 }, end = { row = 3, column = 21 } }
                                , OperatorApplication "*"
                                    Left
                                    ( { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } }
                                    , Integer 2
                                    )
                                    ( { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } }, FunctionOrValue "y" )
                                )
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
    describe "Elm.ProcessingTests"
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
