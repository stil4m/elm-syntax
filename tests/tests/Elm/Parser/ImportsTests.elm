module Elm.Parser.ImportsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Imports as Parser
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ImportTest"
        [ test "import with explicits" <|
            \() ->
                parseFullStringWithNullState "import Foo exposing (Model, Msg(..))" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = Node emptyRange <| [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just <|
                                    Explicit
                                        [ Node emptyRange <| TypeOrAliasExpose "Model"
                                        , Node emptyRange <| TypeExpose (ExposedType "Msg" (Just emptyRange))
                                        ]
                            }
                        )
        , test "import with explicits 2" <|
            \() ->
                parseFullStringWithNullState "import Html exposing (text)" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = Node emptyRange <| [ "Html" ]
                            , moduleAlias = Nothing
                            , exposingList = Just <| Explicit [ Node emptyRange <| FunctionExpose "text" ]
                            }
                        )
        , test "import minimal" <|
            \() ->
                parseFullStringWithNullState "import Foo" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = Node emptyRange <| [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList = Nothing
                            }
                        )
        , test "import with alias" <|
            \() ->
                parseFullStringWithNullState "import Foo as Bar" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = Node emptyRange <| [ "Foo" ]
                            , moduleAlias = Just <| Node emptyRange <| [ "Bar" ]
                            , exposingList = Nothing
                            }
                        )
        ]
