module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Case expression tests"
        [ test "should fail to parse when the matched expression has the wrong indentation" <|
            \() ->
                """case
True
  of
    A -> 1"""
                    |> expectInvalid

        -- TODO Make this pass
        --        ,   test "should fail to parse when the `of` keyword has the wrong indentation" <|
        --                \() ->
        --                    """case True
        --of
        --    A -> 1"""
        --                        |> expectInvalid
        , test "should fail to parse a branch at the start of a line" <|
            \() ->
                """case True of
True -> 1"""
                    |> expectInvalid
        , test "should fail to parse when branch body starts at the start of a line" <|
            \() ->
                """case f of
  True ->
1"""
                    |> expectInvalid
        , test "case expression" <|
            \() ->
                """case f of
  True -> 1
  False -> 2"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                            (CaseExpression
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , cases =
                                    [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                            NamedPattern (QualifiedNameRef [] "True") []
                                      , Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                            Integer 1
                                      )
                                    , ( Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                            NamedPattern (QualifiedNameRef [] "False") []
                                      , Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
                                            Integer 2
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression with trailing whitespace" <|
            \() ->
                """case f of
  True -> 1
  False -> 2

"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                            (CaseExpression
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , cases =
                                    [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                            NamedPattern (QualifiedNameRef [] "True") []
                                      , Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                            Integer 1
                                      )
                                    , ( Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                            NamedPattern (QualifiedNameRef [] "False") []
                                      , Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
                                            Integer 2
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression with qualified imports" <|
            \() ->
                """case f of
  Foo.Bar -> 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } }
                            (CaseExpression
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , cases =
                                    [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                            NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") []
                                      , Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } <|
                                            Integer 1
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression with no space between pattern and value" <|
            \() ->
                """case f of
  x->1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                            (CaseExpression
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , cases =
                                    [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } <|
                                            VarPattern "x"
                                      , Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } <|
                                            Integer 1
                                      )
                                    ]
                                }
                            )
                        )

        -- TODO Make this pass
        --        , test "should parse case expression with first branch on the same line as case of" <|
        --            \() ->
        --                """
        --case x of True -> 1
        --          False -> 2
        --"""
        --                    |> expectAst
        --                        -- AST is incorrect, but this should parse to something correct
        --                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } } UnitExpr)
        , test "should fail to parse case expression with second branch indented differently than the first line (before)" <|
            \() ->
                expectInvalid """case f of
  True -> 1
 False -> 2"""
        , test "should fail to parse case expression with second branch indented differently than the first line (after)" <|
            \() ->
                """case f of
  True -> 1
   False -> 2
"""
                    |> expectInvalid
        , test "should parse case expression when " <|
            \() ->
                """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 6, column = 6 } }
                            (CaseExpression
                                { cases =
                                    [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                      , Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Integer 1)
                                      )
                                    , ( Node { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                      , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Integer 2)
                                      )
                                    ]
                                , expression = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } } (FunctionOrValue [] "msg")
                                }
                            )
                        )
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
