module Elm.Parser.ImportsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Imports as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ImportTest"
        [ test "import with explicits" <|
            \() ->
                parseFullStringWithNullState "import Foo exposing (Model, Msg(..))" Parser.importDefinition
                    |> Maybe.map (unRanged noRangeImport)
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                { moduleName = Node empty <| [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just <|
                                        Node empty <|
                                            Explicit
                                                [ Node empty <| TypeOrAliasExpose "Model"
                                                , Node empty <| TypeExpose (ExposedType "Msg" (Just empty))
                                                ]
                                }
                        )
        , test "import with explicits 2" <|
            \() ->
                parseFullStringWithNullState "import Html exposing (text)" Parser.importDefinition
                    |> Maybe.map (unRanged noRangeImport)
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                { moduleName = Node empty <| [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList = Just <| Node empty <| Explicit [ Node empty <| FunctionExpose "text" ]
                                }
                        )
        , test "import minimal" <|
            \() ->
                parseFullStringWithNullState "import Foo" Parser.importDefinition
                    |> Maybe.map (unRanged noRangeImport)
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                { moduleName = Node empty <| [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                        )
        , test "import with alias" <|
            \() ->
                parseFullStringWithNullState "import Foo as Bar" Parser.importDefinition
                    |> Maybe.map (unRanged noRangeImport)
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                { moduleName = Node empty <| [ "Foo" ]
                                , moduleAlias = Just <| Node empty <| [ "Bar" ]
                                , exposingList = Nothing
                                }
                        )
        , test "import should be non greedy with spacing" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "import Foo\nimport X" Parser.importDefinition
                    |> Expect.equal
                        (Just
                            (Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                { moduleName = Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } } <| [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                        )
        ]
