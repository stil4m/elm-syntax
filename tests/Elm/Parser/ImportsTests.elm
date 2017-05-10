module Elm.Parser.ImportsTests exposing (..)

import Elm.Parser.Imports as Parser
import Elm.Syntax.Range exposing (..)
import Test exposing (..)
import Expect
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Syntax.Exposing exposing (..)


all : Test
all =
    describe "ImportTest"
        [ test "import with explicits" <|
            \() ->
                parseFullStringWithNullState "import Foo exposing (Model, Msg(..))" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Explicit
                                    [ TypeOrAliasExpose "Model" emptyRange
                                    , TypeExpose (ExposedType "Msg" (All emptyRange) emptyRange)
                                    ]
                            , range = emptyRange
                            }
                        )
        , test "import with explicits 2" <|
            \() ->
                parseFullStringWithNullState "import Html exposing (text)" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = [ "Html" ]
                            , moduleAlias = Nothing
                            , exposingList = Explicit [ FunctionExpose "text" emptyRange ]
                            , range = emptyRange
                            }
                        )
        , test "import minimal" <|
            \() ->
                parseFullStringWithNullState "import Foo" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList = None
                            , range = emptyRange
                            }
                        )
        , test "import with alias" <|
            \() ->
                parseFullStringWithNullState "import Foo as Bar" Parser.importDefinition
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = [ "Foo" ]
                            , moduleAlias = Just [ "Bar" ]
                            , exposingList = None
                            , range = emptyRange
                            }
                        )
        ]
