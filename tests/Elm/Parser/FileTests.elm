module Elm.Parser.FileTests exposing (all)

import Elm.Parser
import Elm.Parser.File as Parser
import Elm.Parser.Samples as Samples
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import ParserFast
import Test exposing (..)


all : Test
all =
    Test.concat
        [ describe "FileTests" <|
            List.map
                (\( n, s ) ->
                    test ("sample " ++ String.fromInt n) <|
                        \() ->
                            case ParserFast.run Parser.file s of
                                Err error ->
                                    Expect.fail (error |> Debug.toString)

                                Ok _ ->
                                    Expect.pass
                )
                Samples.allSamples

        -- , describe "Error messages" <|
        --     [ test "failure on module name" <|
        --         \() ->
        --             Parser.parse "module foo exposing (..)\nx = 1"
        --                 |> Result.toMaybe
        --                 |> Expect.equal Nothing
        --     , test "failure on declaration" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..)\n\ntype x = \n  1"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,0)" ])
        --     , test "failure on declaration expression" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..) \nx = \n  x + _"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,6)" ])
        --     ]
        , test "Comments ordering" <|
            \() ->
                let
                    input : String
                    input =
                        """
module Foo exposing (..)

{-| Module documentation
-}

import A

-- 1
{- 2 -}
-- 3

{-| Function declaration
-}
f =
    -- 4
    identity

-- 5
{- 6 -}
"""
                in
                Elm.Parser.parse input
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
                            ]
                        )
        , test "declarations with comments" <|
            \() ->
                """module Foo exposing (b, fn)

fn a =
    case a of
        X ->
            1
                -- 1
                + 2

        Y ->
            1

-- 2

b =
    1

"""
                    |> Elm.Parser.parse
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Node { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } } "-- 1"
                            , Node { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } } "-- 2"
                            ]
                        )
        , test "function declaration with a case and trailing whitespace" <|
            \() ->
                """
module Trailing.Whitespace exposing (..)

caseWhitespace f = case f   of
  True     -> 
    1   
  False   
     -> 
     
     
         2    

   --some comment

    
    """
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                , arguments = [ Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (VarPattern_ "f") ]
                                                , expression =
                                                    Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (Case
                                                            { expression = Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (FunctionOrValue [] "f")
                                                            , firstCase =
                                                                ( Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (IntegerLiteral 1)
                                                                )
                                                            , restOfCases =
                                                                [ ( Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (NamedPattern { moduleName = [], name = "False" } [])
                                                                  , Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (IntegerLiteral 2)
                                                                  )
                                                                ]
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
                            }
                        )
        , test "function declaration with lambda and trailing whitespace" <|
            \() ->
                """
module Trailing.Whitespace exposing (..)

lambdaWhitespace =   \\ a b ->    a    

       + 

    b 


--some comment

    """
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (LambdaExpression
                                                            { firstArg = Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (VarPattern_ "a")
                                                            , restOfArgs =
                                                                [ Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (VarPattern_ "b") ]
                                                            , expression =
                                                                Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (Operation "+"
                                                                        Left
                                                                        (Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (FunctionOrValue [] "a"))
                                                                        (Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
        , test "function declaration with let and trailing whitespace" <|
            \() ->
                """
module Trailing.Whitespace exposing (..)

letWhitespace = let
                  b =   1

 in
 b


--some comment

    """
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (LetExpression
                                                            { declarations =
                                                                [ Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { name = Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                , arguments = []
                                                                                , expression = Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (IntegerLiteral 1)
                                                                                }
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (FunctionOrValue [] "b")
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
        , test "type declaration with documentation after imports" <|
            \() ->
                """
module Foo exposing (..)

import Dict

{-| Config goes here
-}
type Configuration
    = Configuration
"""
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (NormalModule
                                        { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { moduleName = Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (CustomTypeDeclaration
                                        { documentation = Just (Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , name = Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        , generics = []
                                        , firstConstructor =
                                            Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { name = Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                        , restOfConstructors = []
                                        }
                                    )
                                ]
                            , comments = []
                            }
                        )
        , test "module documentation formatted like a type documentation" <|
            \() ->
                """
module Foo exposing (..)

{-| actually module doc
-}
type Configuration
    = Configuration
"""
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (NormalModule
                                        { moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        , generics = []
                                        , firstConstructor =
                                            Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { name = Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                        , restOfConstructors = []
                                        }
                                    )
                                ]
                            , comments = [ Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
                            }
                        )
        , test "documentation on a port declaration" <|
            \() ->
                """
port module Foo exposing (..)

import String

{-| foo
-}
port sendResponse : String -> Cmd msg
"""
                    |> Elm.Parser.parse
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                    (PortModule
                                        { moduleName = Node { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } } [ "Foo" ]
                                        , exposingList =
                                            Node { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                                (All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } })
                                        }
                                    )
                            , imports =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                    { moduleName = Node { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } } [ "String" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Node { start = { row = 6, column = 1 }, end = { row = 8, column = 38 } }
                                    (PortDeclaration
                                        { documentation = Just (Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| foo\n-}")
                                        , signature =
                                            Node { start = { row = 8, column = 6 }, end = { row = 8, column = 38 } }
                                                { name = Node { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } } "sendResponse"
                                                , typeAnnotation =
                                                    Node { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                        (FunctionTypeAnnotation
                                                            (Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                                (Type (Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } } ( [], "String" )) [])
                                                            )
                                                            (Node { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                                (Type (Node { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } } ( [], "Cmd" ))
                                                                    [ Node { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } } (Var "msg") ]
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
        ]
