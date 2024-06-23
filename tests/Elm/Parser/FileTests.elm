module Elm.Parser.FileTests exposing (all)

import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as Parser
import Elm.Parser.Samples as Samples
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
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
        , test "function declarations with trailing whitespace" <|
            \() ->
                """
module Trailing.Whitespace exposing (..)

caseWhitespace f = case f   of
  True     -> 
    1   
  False   
     -> 
     
     
         2    


lambdaWhitespace =   \\ a b ->    a    

       + 

    b 


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
                            , comments = [ Node { start = { row = 28, column = 1 }, end = { row = 28, column = 15 } } "--some comment" ]
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
                                , Node { start = { row = 14, column = 1 }, end = { row = 18, column = 6 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 14, column = 1 }, end = { row = 18, column = 6 } }
                                                { arguments = []
                                                , expression =
                                                    Node { start = { row = 14, column = 22 }, end = { row = 18, column = 6 } }
                                                        (LambdaExpression
                                                            { args =
                                                                [ Node { start = { row = 14, column = 24 }, end = { row = 14, column = 25 } } (VarPattern "a")
                                                                , Node { start = { row = 14, column = 26 }, end = { row = 14, column = 27 } } (VarPattern "b")
                                                                ]
                                                            , expression =
                                                                Node { start = { row = 14, column = 34 }, end = { row = 18, column = 6 } }
                                                                    (OperatorApplication "+"
                                                                        Left
                                                                        (Node { start = { row = 14, column = 34 }, end = { row = 14, column = 35 } } (FunctionOrValue [] "a"))
                                                                        (Node { start = { row = 18, column = 5 }, end = { row = 18, column = 6 } } (FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                , name = Node { start = { row = 14, column = 1 }, end = { row = 14, column = 17 } } "lambdaWhitespace"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                , Node { start = { row = 21, column = 1 }, end = { row = 25, column = 3 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 21, column = 1 }, end = { row = 25, column = 3 } }
                                                { arguments = []
                                                , expression =
                                                    Node { start = { row = 21, column = 17 }, end = { row = 25, column = 3 } }
                                                        (LetExpression
                                                            { declarations =
                                                                [ Node { start = { row = 22, column = 19 }, end = { row = 22, column = 26 } }
                                                                    (LetFunction
                                                                        { declaration =
                                                                            Node { start = { row = 22, column = 19 }, end = { row = 22, column = 26 } }
                                                                                { arguments = []
                                                                                , expression = Node { start = { row = 22, column = 25 }, end = { row = 22, column = 26 } } (Integer 1)
                                                                                , name = Node { start = { row = 22, column = 19 }, end = { row = 22, column = 20 } } "b"
                                                                                }
                                                                        , documentation = Nothing
                                                                        , signature = Nothing
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Node { start = { row = 25, column = 2 }, end = { row = 25, column = 3 } } (FunctionOrValue [] "b")
                                                            }
                                                        )
                                                , name = Node { start = { row = 21, column = 1 }, end = { row = 21, column = 14 } } "letWhitespace"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            }
                        )
        ]
