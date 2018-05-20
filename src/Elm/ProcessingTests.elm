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
                , exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                }
      , imports = []
      , declarations =
            [ ( { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
              , FuncDecl
                    { documentation = Just { text = "{-| The docs\n-}", range = { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } } }
                    , signature = Nothing
                    , declaration =
                        { name = { value = "bar", range = { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } }
                        , arguments = []
                        , expression = ( { start = { row = 5, column = 7 }, end = { row = 5, column = 8 } }, Integer 1 )
                        }
                    }
              )
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
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                }
      , imports = []
      , declarations =
            [ ( { start = { row = 5, column = 1 }, end = { row = 6, column = 8 } }
              , FuncDecl
                    { documentation =
                        Just
                            { text = "{-| The docs\n-}"
                            , range = { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }
                            }
                    , signature =
                        Just
                            ( { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } }
                            , { name =
                                    { value = "bar"
                                    , range = { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } }
                                    }
                              , typeAnnotation =
                                    ( { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } }
                                    , Typed [] "Int" []
                                    )
                              }
                            )
                    , declaration =
                        { name = { value = "bar", range = { start = { row = 6, column = 1 }, end = { row = 6, column = 4 } } }
                        , arguments = []
                        , expression = ( { start = { row = 6, column = 7 }, end = { row = 6, column = 8 } }, Integer 1 )
                        }
                    }
              )
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
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                }
      , imports = []
      , declarations =
            [ ( { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
              , FuncDecl
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        { name = { value = "bar", range = { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } }
                        , arguments = []
                        , expression = ( { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } }, Integer 1 )
                        }
                    }
              )
            ]
      , comments = [ ( { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } }, "--The Doc" ) ]
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
    , { moduleDefinition = NormalModule { moduleName = [ "Bar" ], exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } } }
      , imports = []
      , declarations =
            [ ( { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
              , FuncDecl
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        { name = { value = "bar", range = { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } }
                        , arguments = []
                        , expression = ( { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } }, Integer 1 )
                        }
                    }
              )
            ]
      , comments = [ ( { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }, "{- The Doc -}" ) ]
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
                , exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                }
      , imports = []
      , declarations =
            [ ( { start = { row = 4, column = 1 }, end = { row = 5, column = 23 } }
              , AliasDecl
                    { documentation =
                        Just
                            { text = "{-| The Doc -}"
                            , range = { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                            }
                    , name = "Foo"
                    , generics = []
                    , typeAnnotation =
                        ( { start = { row = 5, column = 6 }, end = { row = 5, column = 23 } }
                        , Record
                            [ ( "name"
                              , ( { start = { row = 5, column = 15 }, end = { row = 5, column = 21 } }
                                , Typed [] "String" []
                                )
                              )
                            ]
                        )
                    }
              )
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
    , { moduleDefinition =
            NormalModule
                { moduleName = [ "Bar" ]
                , exposingList = All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                }
      , imports = []
      , declarations =
            [ ( { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
              , FuncDecl
                    { documentation = Nothing
                    , signature = Nothing
                    , declaration =
                        { name = { value = "bar", range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } }
                        , arguments = []
                        , expression =
                            ( { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } }
                            , OperatorApplication "*"
                                Left
                                ( { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                , ParenthesizedExpression
                                    ( { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                    , OperatorApplication "+"
                                        Left
                                        ( { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }
                                        , FunctionOrValue "x"
                                        )
                                        ( { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }, Integer 1 )
                                    )
                                )
                                ( { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } }
                                , ParenthesizedExpression
                                    ( { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } }
                                    , OperatorApplication "*"
                                        Left
                                        ( { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } }, Integer 2 )
                                        ( { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } }, FunctionOrValue "y" )
                                    )
                                )
                            )
                        }
                    }
              )
            ]
      , comments = []
      }
    )


main =
    Tuple.second suite


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
            ]
        )
