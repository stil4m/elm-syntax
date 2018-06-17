module Elm.Parser.PatternTests exposing (..)

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
        [ test "all pattern" <|
            \() ->
                parseFullStringState emptyState "_" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 1), AllPattern ))
        , test "unit pattern" <|
            \() ->
                parseFullStringState emptyState "()" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 2), UnitPattern ))
        , test "string pattern" <|
            \() ->
                parseFullStringState emptyState "\"Foo\"" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 5), StringPattern "Foo" ))
        , test "char pattern" <|
            \() ->
                parseFullStringState emptyState "'f'" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 3), CharPattern 'f' ))
        , test "non cons pattern " <|
            \() ->
                parseFullStringState emptyState "(X x)" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 0 0) (Location 0 5)
                            , ParenthesizedPattern
                                ( Range (Location 0 1) (Location 0 4)
                                , NamedPattern (QualifiedNameRef [] "X")
                                    [ ( Range (Location 0 3) (Location 0 4), VarPattern "x" ) ]
                                )
                            )
                        )
        , test "uncons with parens pattern" <|
            \() ->
                parseFullStringState emptyState "(X x) :: xs" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 0 0) (Location 0 11)
                            , UnConsPattern
                                ( Range (Location 0 0) (Location 0 5)
                                , ParenthesizedPattern
                                    ( Range (Location 0 1) (Location 0 4)
                                    , NamedPattern (QualifiedNameRef [] "X")
                                        [ ( Range (Location 0 3) (Location 0 4), VarPattern "x" ) ]
                                    )
                                )
                                ( Range (Location 0 9) (Location 0 11), VarPattern "xs" )
                            )
                        )
        , test "int pattern" <|
            \() ->
                parseFullStringState emptyState "1" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 1), IntPattern 1 ))
        , test "uncons pattern" <|
            \() ->
                parseFullStringState emptyState "n :: tail" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 0 0) (Location 0 9)
                            , UnConsPattern ( Range (Location 0 0) (Location 0 1), VarPattern "n" )
                                ( Range (Location 0 5) (Location 0 9), VarPattern "tail" )
                            )
                        )
        , test "list pattern" <|
            \() ->
                parseFullStringState emptyState "[1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            ( Range (Location 0 0) (Location 0 3)
                            , ListPattern [ ( Range (Location 0 1) (Location 0 2), IntPattern 1 ) ]
                            )
                        )
        , test "float pattern" <|
            \() ->
                parseFullStringState emptyState "1.2" Parser.pattern
                    |> Expect.equal (Just ( Range (Location 0 0) (Location 0 3), FloatPattern 1.2 ))
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
                            ( Range (Location 0 0) (Location 0 27)
                            , AsPattern
                                ( Range (Location 0 0) (Location 0 15)
                                , RecordPattern
                                    [ { value = "model", range = Range (Location 0 1) (Location 0 6) }
                                    , { value = "context", range = Range (Location 0 7) (Location 0 14) }
                                    ]
                                )
                                { value = "appState", range = Range (Location 0 19) (Location 0 27) }
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
                                        ( emptyRange
                                        , NamedPattern
                                            (QualifiedNameRef [] "Index")
                                            [ ( emptyRange, VarPattern "irec" ) ]
                                        )
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
