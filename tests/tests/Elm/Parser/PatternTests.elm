module Elm.Parser.PatternTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Patterns as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "PatternTests"
        [ test "unit pattern" <|
            \() ->
                parseFullStringState emptyState "()" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 3), UnitPattern ))
        , test "string pattern" <|
            \() ->
                parseFullStringState emptyState "\"Foo\"" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 6), StringPattern "Foo" ))
        , test "multiple patterns" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.pattern
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just (VarPattern "a"))
        , test "char pattern" <|
            \() ->
                parseFullStringState emptyState "'f'" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 4), CharPattern 'f' ))
        , test "qualified pattern" <|
            \() ->
                parseFullStringState emptyState "X x" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 4)
                            , NamedPattern (QualifiedNameRef [] "X")
                                [ ( Range (Location 1 3) (Location 1 4), VarPattern "x" ) ]
                            )
                        )
        , test "quaified pattern without and with spacing should parse to the same" <|
            \() ->
                let
                    a =
                        parseFullStringWithNullState "Bar " Parser.pattern

                    b =
                        parseFullStringWithNullState "Bar" Parser.pattern
                in
                a
                    |> Expect.equal b
        , test "all pattern" <|
            \() ->
                parseFullStringState emptyState "_" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 2), AllPattern ))
        , test "non cons pattern " <|
            \() ->
                parseFullStringState emptyState "(X x)" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 6)
                            , ParenthesizedPattern
                                ( Range (Location 1 2) (Location 1 5)
                                , NamedPattern (QualifiedNameRef [] "X")
                                    [ ( Range (Location 1 4) (Location 1 5), VarPattern "x" ) ]
                                )
                            )
                        )
        , test "uncons with parens pattern" <|
            \() ->
                parseFullStringState emptyState "(X x) :: xs" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 12)
                            , UnConsPattern
                                ( Range (Location 1 1) (Location 1 6)
                                , ParenthesizedPattern
                                    ( Range (Location 1 2) (Location 1 5)
                                    , NamedPattern (QualifiedNameRef [] "X")
                                        [ ( Range (Location 1 4) (Location 1 5), VarPattern "x" ) ]
                                    )
                                )
                                ( Range (Location 1 10) (Location 1 12), VarPattern "xs" )
                            )
                        )
        , test "int pattern" <|
            \() ->
                parseFullStringState emptyState "1" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 2), IntPattern 1 ))
        , test "uncons pattern" <|
            \() ->
                parseFullStringState emptyState "n :: tail" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 10)
                            , UnConsPattern ( Range (Location 1 1) (Location 1 2), VarPattern "n" )
                                ( Range (Location 1 6) (Location 1 10), VarPattern "tail" )
                            )
                        )
        , test "list pattern" <|
            \() ->
                parseFullStringState emptyState "[1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 4)
                            , ListPattern [ ( Range (Location 1 2) (Location 1 3), IntPattern 1 ) ]
                            )
                        )
        , test "float pattern" <|
            \() ->
                parseFullStringState emptyState "1.2" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 1 1) (Location 1 4), FloatPattern 1.2 ))
        , test "record pattern" <|
            \() ->
                parseFullStringState emptyState "{a,b}" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , RecordPattern
                                [ { value = "a", range = emptyRange }
                                , { value = "b", range = emptyRange }
                                ]
                            )
                        )
        , test "named pattern" <|
            \() ->
                parseFullStringState emptyState "True" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal (Just ( emptyRange, NamedPattern (QualifiedNameRef [] "True") [] ))
        , test "tuple pattern" <|
            \() ->
                parseFullStringState emptyState "(a,{b,c},())" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , TuplePattern
                                [ ( emptyRange, VarPattern "a" )
                                , ( emptyRange
                                  , RecordPattern
                                        [ { value = "b", range = emptyRange }
                                        , { value = "c", range = emptyRange }
                                        ]
                                  )
                                , ( emptyRange, UnitPattern )
                                ]
                            )
                        )
        , test "destructure pattern" <|
            \() ->
                parseFullStringState emptyState "Set x" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , NamedPattern (QualifiedNameRef [] "Set")
                                [ ( emptyRange, VarPattern "x" ) ]
                            )
                        )
        , test "tuple pattern 2" <|
            \() ->
                parseFullStringState emptyState "(model, cmd)" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , TuplePattern
                                [ ( emptyRange, VarPattern "model" )
                                , ( emptyRange, VarPattern "cmd" )
                                ]
                            )
                        )
        , test "record as pattern" <|
            \() ->
                parseFullStringState emptyState "{model,context} as appState" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 1 1) (Location 1 28)
                            , AsPattern
                                ( Range (Location 1 1) (Location 1 16)
                                , RecordPattern
                                    [ { value = "model", range = Range (Location 1 2) (Location 1 7) }
                                    , { value = "context", range = Range (Location 1 8) (Location 1 15) }
                                    ]
                                )
                                { value = "appState", range = Range (Location 1 20) (Location 1 28) }
                            )
                        )
        , test "complex pattern" <|
            \() ->
                parseFullStringState emptyState "(Index irec as index, docVector)" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , TuplePattern
                                [ ( emptyRange
                                  , AsPattern
                                        ( emptyRange, NamedPattern (QualifiedNameRef [] "Index") [ ( emptyRange, VarPattern "irec" ) ] )
                                        { value = "index", range = emptyRange }
                                  )
                                , ( emptyRange, VarPattern "docVector" )
                                ]
                            )
                        )
        , test "complex pattern 2" <|
            \() ->
                parseFullStringState emptyState "RBNode_elm_builtin col (RBNode_elm_builtin Red  (RBNode_elm_builtin Red xv))" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                [ ( emptyRange, VarPattern "col" )
                                , ( emptyRange
                                  , ParenthesizedPattern
                                        ( emptyRange
                                        , NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                            [ ( emptyRange, NamedPattern (QualifiedNameRef [] "Red") [] )
                                            , ( emptyRange
                                              , ParenthesizedPattern
                                                    ( emptyRange
                                                    , NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                                        [ ( emptyRange, NamedPattern (QualifiedNameRef [] "Red") [] )
                                                        , ( emptyRange, VarPattern "xv" )
                                                        ]
                                                    )
                                              )
                                            ]
                                        )
                                  )
                                ]
                            )
                        )
        ]
