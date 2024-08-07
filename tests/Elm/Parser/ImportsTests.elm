module Elm.Parser.ImportsTests exposing (all)

import Elm.Parser.Imports as Parser
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "ImportTest"
        [ test "import with explicits" <|
            \() ->
                "import Foo exposing (Model, Msg(..))"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just
                                    (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 37 } }
                                        (Explicit
                                            (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 27 } } (TypeOrAliasExpose "Model"))
                                            [ Node { start = { row = 1, column = 29 }, end = { row = 1, column = 36 } } (TypeExpose { name = "Msg", open = Just { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } }) ]
                                        )
                                    )
                            }
                        )
        , test "import with explicits 2" <|
            \() ->
                "import Html exposing (text)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                            , moduleAlias = Nothing
                            , exposingList =
                                Just
                                    (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 28 } }
                                        (Explicit (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 27 } } (FunctionExpose "text")) [])
                                    )
                            }
                        )
        , test "import minimal" <|
            \() ->
                "import Foo"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Nothing
                            , exposingList = Nothing
                            }
                        )
        , test "import with alias" <|
            \() ->
                "import Foo as Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Just (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                            , exposingList = Nothing
                            }
                        )
        , test "import with alias and exposing all" <|
            \() ->
                "import Foo as Bar exposing (..)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            { moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            , moduleAlias = Just (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                            , exposingList =
                                Just
                                    (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                        (All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                    )
                            }
                        )
        , test "import with invalid alias containing ." <|
            \() ->
                "import Foo as Bar.Buzz"
                    |> ParserWithCommentsUtil.expectInvalid Parser.importDefinition
        ]


expectAst : Node Import -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst Parser.importDefinition
