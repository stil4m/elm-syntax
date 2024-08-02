module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.Expression exposing (expression)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.StringLiteralType exposing (StringLiteralType(..))
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
        , test "should fail to parse when the `of` keyword has the wrong indentation" <|
            \() ->
                """case True
of
               A -> 1"""
                    |> expectInvalid
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
                            (Case
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , firstCase =
                                    ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                        NamedPattern (QualifiedNameRef [] "True") []
                                    , Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                        IntegerLiteral 1
                                    )
                                , restOfCases =
                                    [ ( Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                            NamedPattern (QualifiedNameRef [] "False") []
                                      , Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
                                            IntegerLiteral 2
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
                            (Case
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , firstCase =
                                    ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                        NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") []
                                    , Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } <|
                                        IntegerLiteral 1
                                    )
                                , restOfCases = []
                                }
                            )
                        )
        , test "case expression with no space between pattern and value" <|
            \() ->
                """case f of
  x->1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                            (Case
                                { expression =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                        FunctionOrValue [] "f"
                                , firstCase =
                                    ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } <|
                                        VarPattern "x"
                                    , Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } <|
                                        IntegerLiteral 1
                                    )
                                , restOfCases = []
                                }
                            )
                        )
        , test "should parse case expression with first branch on the same line as case of" <|
            \() ->
                """case x of True -> 1
          False -> 2"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 21 } }
                            (Case
                                { expression = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (FunctionOrValue [] "x")
                                , firstCase =
                                    ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } } (NamedPattern { moduleName = [], name = "True" } [])
                                    , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (IntegerLiteral 1)
                                    )
                                , restOfCases =
                                    [ ( Node { start = { row = 2, column = 11 }, end = { row = 2, column = 16 } } (NamedPattern { moduleName = [], name = "False" } [])
                                      , Node { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } } (IntegerLiteral 2)
                                      )
                                    ]
                                }
                            )
                        )
        , test "should parse case expression with a multiline pattern" <|
            \() ->
                """case x of
        \"\"\"single line triple quote\"\"\" ->
            1
        \"\"\"multi line
            triple quote\"\"\" ->
            2
        _ -> 3"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 15 } }
                            (Case
                                { expression =
                                    Node
                                        { start = { row = 1, column = 6 }
                                        , end = { row = 1, column = 7 }
                                        }
                                        (FunctionOrValue [] "x")
                                , firstCase =
                                    ( Node { start = { row = 2, column = 9 }, end = { row = 2, column = 39 } } (StringPattern TripleQuote "single line triple quote")
                                    , Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (IntegerLiteral 1)
                                    )
                                , restOfCases =
                                    [ ( Node { start = { row = 4, column = 9 }, end = { row = 5, column = 28 } } (StringPattern TripleQuote "multi line\n            triple quote")
                                      , Node { start = { row = 6, column = 13 }, end = { row = 6, column = 14 } } (IntegerLiteral 2)
                                      )
                                    , ( Node { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } } AllPattern
                                      , Node { start = { row = 7, column = 14 }, end = { row = 7, column = 15 } } (IntegerLiteral 3)
                                      )
                                    ]
                                }
                            )
                        )
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
                            (Case
                                { expression = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } } (FunctionOrValue [] "msg")
                                , firstCase =
                                    ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                    , Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (IntegerLiteral 1)
                                    )
                                , restOfCases =
                                    [ ( Node { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                      , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (IntegerLiteral 2)
                                      )
                                    ]
                                }
                            )
                        )
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst expression


expectInvalid : String -> Expect.Expectation
expectInvalid =
    ParserWithCommentsUtil.expectInvalid expression
