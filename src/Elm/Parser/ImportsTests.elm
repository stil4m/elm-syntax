module Elm.Parser.ImportsTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Imports as Parser
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "ImportTest"
        [ test "import with explicits" <|
            \() ->
                parseFullStringWithNullState "import Foo exposing (Model, Msg(..))" Parser.importDefinition
                    |> Debug.log "Foo"
                    |> Maybe.map noRangeImport
                    |> Expect.equal
                        (Just
                            { moduleName = [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just <|
                                    Explicit
                                        [ ( emptyRange, TypeOrAliasExpose "Model" )
                                        , ( emptyRange, TypeExpose (ExposedType "Msg" (Just emptyRange)) )
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
                            , exposingList = Just <| Explicit [ ( emptyRange, FunctionExpose "text" ) ]
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
                            , exposingList = Nothing
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
                            , exposingList = Nothing
                            , range = emptyRange
                            }
                        )
        ]
