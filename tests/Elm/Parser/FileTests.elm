module Elm.Parser.FileTests exposing (all)

import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as Parser
import Elm.Parser.Samples as Samples
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import Json.Decode
import Json.Encode
import Test exposing (..)


all : Test
all =
    Test.concat
        [ describe "FileTests" <|
            List.indexedMap
                (\n s ->
                    test ("sample " ++ String.fromInt (n + 1)) <|
                        \() ->
                            parse s Parser.file |> Expect.notEqual Nothing
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
        , describe "FileTests - serialisation"
            (Samples.allSamples
                |> List.indexedMap
                    (\n s ->
                        test ("sample " ++ String.fromInt (n + 1)) <|
                            \() ->
                                let
                                    parsed : Maybe RawFile
                                    parsed =
                                        parse s Parser.file
                                            |> Maybe.map InternalRawFile.Raw

                                    roundTrip : Maybe RawFile
                                    roundTrip =
                                        parsed
                                            |> Maybe.map (RawFile.encode >> Json.Encode.encode 0)
                                            |> Maybe.andThen (Json.Decode.decodeString RawFile.decoder >> Result.toMaybe)
                                in
                                Expect.equal parsed roundTrip
                    )
            )
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
                Elm.Parser.parseToFile input
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        }
                                    )
                            , comments = [ Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { arguments = [ Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (VarPattern "f") ]
                                                , expression =
                                                    Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (CaseExpression
                                                            { cases =
                                                                [ ( Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                  , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Integer 1)
                                                                  )
                                                                , ( Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (NamedPattern { moduleName = [], name = "False" } [])
                                                                  , Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (Integer 2)
                                                                  )
                                                                ]
                                                            , expression = Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (FunctionOrValue [] "f")
                                                            }
                                                        )
                                                , name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        }
                                    )
                            , comments = [ Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { arguments = []
                                                , expression =
                                                    Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (LambdaExpression
                                                            { args =
                                                                [ Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (VarPattern "a")
                                                                , Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (VarPattern "b")
                                                                ]
                                                            , expression =
                                                                Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (OperatorApplication "+"
                                                                        Left
                                                                        (Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (FunctionOrValue [] "a"))
                                                                        (Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                , name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        }
                                    )
                            , comments = [ Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { arguments = []
                                                , expression =
                                                    Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (LetExpression
                                                            { declarations =
                                                                [ Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (LetFunction
                                                                        { declaration =
                                                                            Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { arguments = []
                                                                                , expression = Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (Integer 1)
                                                                                , name = Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                }
                                                                        , documentation = Nothing
                                                                        , signature = Nothing
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (FunctionOrValue [] "b")
                                                            }
                                                        )
                                                , name = Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { comments = []
                            , declarations =
                                [ Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (CustomTypeDeclaration
                                        { constructors =
                                            [ Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { arguments = []
                                                , name = Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
                                                }
                                            ]
                                        , documentation = Just (Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , generics = []
                                        , name = Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        }
                                    )
                                ]
                            , imports =
                                [ Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { exposingList = Nothing
                                    , moduleAlias = Nothing
                                    , moduleName = Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    }
                                ]
                            , moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        }
                                    )
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
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { comments = [ Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
                            , declarations =
                                [ Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (CustomTypeDeclaration
                                        { constructors =
                                            [ Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { arguments = []
                                                , name = Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                }
                                            ]
                                        , documentation = Nothing
                                        , generics = []
                                        , name = Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        }
                                    )
                            }
                        )
        , test "documentation on a port declaration is treated as a normal comment" <|
            \() ->
                """
port module Foo exposing (..)

import String

{-| foo
-}
port sendResponse : String -> Cmd msg
"""
                    |> Elm.Parser.parseToFile
                    |> Expect.equal
                        (Ok
                            { comments = [ Node { end = { column = 3, row = 7 }, start = { column = 1, row = 6 } } "{-| foo\n-}" ]
                            , declarations =
                                [ Node { end = { column = 38, row = 8 }, start = { column = 1, row = 8 } }
                                    (PortDeclaration
                                        { name = Node { end = { column = 18, row = 8 }, start = { column = 6, row = 8 } } "sendResponse"
                                        , typeAnnotation =
                                            Node { end = { column = 38, row = 8 }, start = { column = 21, row = 8 } }
                                                (FunctionTypeAnnotation
                                                    (Node { end = { column = 27, row = 8 }, start = { column = 21, row = 8 } }
                                                        (Typed (Node { end = { column = 27, row = 8 }, start = { column = 21, row = 8 } } ( [], "String" )) [])
                                                    )
                                                    (Node { end = { column = 38, row = 8 }, start = { column = 31, row = 8 } }
                                                        (Typed (Node { end = { column = 34, row = 8 }, start = { column = 31, row = 8 } } ( [], "Cmd" ))
                                                            [ Node { end = { column = 38, row = 8 }, start = { column = 35, row = 8 } } (GenericType "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , imports =
                                [ Node { end = { column = 14, row = 4 }, start = { column = 1, row = 4 } }
                                    { exposingList = Nothing
                                    , moduleAlias = Nothing
                                    , moduleName = Node { end = { column = 14, row = 4 }, start = { column = 8, row = 4 } } [ "String" ]
                                    }
                                ]
                            , moduleDefinition =
                                Node { end = { column = 30, row = 2 }, start = { column = 1, row = 2 } }
                                    (PortModule
                                        { exposingList =
                                            Node { end = { column = 30, row = 2 }, start = { column = 17, row = 2 } }
                                                (All { end = { column = 29, row = 2 }, start = { column = 27, row = 2 } })
                                        , moduleName = Node { end = { column = 16, row = 2 }, start = { column = 13, row = 2 } } [ "Foo" ]
                                        }
                                    )
                            }
                        )
        ]
