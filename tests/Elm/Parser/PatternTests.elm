module Elm.Parser.PatternTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Patterns as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Location, Range, empty)
import Expect
import Test exposing (..)


all : Test
all =
    describe "PatternTests"
        [ test "unit pattern" <|
            \() ->
                parseFullStringState emptyState "()" Parser.pattern
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 3)) UnitPattern))
        , test "string pattern" <|
            \() ->
                parseFullStringState emptyState "\"Foo\"" Parser.pattern
                    |> Expect.equal (Just <| Node (Range (Location 1 1) (Location 1 6)) (StringPattern "Foo"))
        , test "multiple patterns" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.pattern
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just (VarPattern "a"))
        , test "char pattern" <|
            \() ->
                parseFullStringState emptyState "'f'" Parser.pattern
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 4)) (CharPattern 'f')))
        , test "qualified pattern" <|
            \() ->
                parseFullStringState emptyState "X x" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 4))
                                (NamedPattern (QualifiedNameRef [] "X")
                                    [ Node (Range (Location 1 3) (Location 1 4)) (VarPattern "x") ]
                                )
                            )
                        )
        , test "qualified pattern without and with spacing should parse to the same" <|
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
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 2)) AllPattern))
        , test "non cons pattern " <|
            \() ->
                parseFullStringState emptyState "(X x)" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 6))
                                (ParenthesizedPattern
                                    (Node (Range (Location 1 2) (Location 1 5))
                                        (NamedPattern (QualifiedNameRef [] "X")
                                            [ Node (Range (Location 1 4) (Location 1 5)) (VarPattern "x") ]
                                        )
                                    )
                                )
                            )
                        )
        , test "uncons with parens pattern" <|
            \() ->
                parseFullStringState emptyState "(X x) :: xs" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 12)) <|
                                UnConsPattern
                                    (Node (Range (Location 1 1) (Location 1 6)) <|
                                        ParenthesizedPattern
                                            (Node (Range (Location 1 2) (Location 1 5)) <|
                                                NamedPattern (QualifiedNameRef [] "X")
                                                    [ Node (Range (Location 1 4) (Location 1 5)) <| VarPattern "x" ]
                                            )
                                    )
                                    (Node (Range (Location 1 10) (Location 1 12)) <| VarPattern "xs")
                            )
                        )
        , test "int pattern" <|
            \() ->
                parseFullStringState emptyState "1" Parser.pattern
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 2)) <| IntPattern 1))
        , test "uncons pattern" <|
            \() ->
                parseFullStringState emptyState "n :: tail" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 10)) <|
                                UnConsPattern (Node (Range (Location 1 1) (Location 1 2)) <| VarPattern "n")
                                    (Node (Range (Location 1 6) (Location 1 10)) <| VarPattern "tail")
                            )
                        )
        , test "list pattern" <|
            \() ->
                parseFullStringState emptyState "[1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 4)) <|
                                ListPattern [ Node (Range (Location 1 2) (Location 1 3)) <| IntPattern 1 ]
                            )
                        )
        , test "empty list pattern" <|
            \() ->
                parseFullStringState emptyState "[]" Parser.pattern
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 3)) (ListPattern [])))
        , test "empty list pattern with whitespace" <|
            \() ->
                parseFullStringState emptyState "[ ]" Parser.pattern
                    |> Expect.equal (Just (Node (Range (Location 1 1) (Location 1 4)) (ListPattern [])))
        , test "single element list pattern with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState "[1 ]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 5)) <|
                                ListPattern [ Node (Range (Location 1 2) (Location 1 3)) <| IntPattern 1 ]
                            )
                        )
        , test "single element list pattern with leading whitespace" <|
            \() ->
                parseFullStringState emptyState "[ 1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 5)) <|
                                ListPattern [ Node (Range (Location 1 3) (Location 1 4)) <| IntPattern 1 ]
                            )
                        )
        , test "record pattern" <|
            \() ->
                parseFullStringState emptyState "{a,b}" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                RecordPattern
                                    [ Node empty "a"
                                    , Node empty "b"
                                    ]
                            )
                        )
        , test "record pattern with whitespace" <|
            \() ->
                parseFullStringState emptyState "{a , b}" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                RecordPattern
                                    [ Node empty "a"
                                    , Node empty "b"
                                    ]
                            )
                        )
        , test "record pattern with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState "{a }" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                RecordPattern
                                    [ Node empty "a"
                                    ]
                            )
                        )
        , test "record pattern with leading whitespace" <|
            \() ->
                parseFullStringState emptyState "{ a}" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                RecordPattern
                                    [ Node empty "a"
                                    ]
                            )
                        )
        , test "empty record pattern" <|
            \() ->
                parseFullStringState emptyState "{}" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal (Just (Node empty <| RecordPattern []))
        , test "empty record pattern with whitespace" <|
            \() ->
                parseFullStringState emptyState "{ }" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal (Just (Node empty <| RecordPattern []))
        , test "named pattern" <|
            \() ->
                parseFullStringState emptyState "True" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal (Just (Node empty <| NamedPattern (QualifiedNameRef [] "True") []))
        , test "tuple pattern" <|
            \() ->
                parseFullStringState emptyState "(a,{b,c},())" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                TuplePattern
                                    [ Node empty <| VarPattern "a"
                                    , Node empty <|
                                        RecordPattern
                                            [ Node empty "b"
                                            , Node empty "c"
                                            ]
                                    , Node empty UnitPattern
                                    ]
                            )
                        )
        , test "destructure pattern" <|
            \() ->
                parseFullStringState emptyState "Set x" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                NamedPattern (QualifiedNameRef [] "Set")
                                    [ Node empty <| VarPattern "x" ]
                            )
                        )
        , test "tuple pattern 2" <|
            \() ->
                parseFullStringState emptyState "(model, cmd)" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                TuplePattern
                                    [ Node empty <| VarPattern "model"
                                    , Node empty <| VarPattern "cmd"
                                    ]
                            )
                        )
        , test "record as pattern" <|
            \() ->
                parseFullStringState emptyState "{model,context} as appState" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node (Range (Location 1 1) (Location 1 28)) <|
                                AsPattern
                                    (Node (Range (Location 1 1) (Location 1 16)) <|
                                        RecordPattern
                                            [ Node (Range (Location 1 2) (Location 1 7)) "model"
                                            , Node (Range (Location 1 8) (Location 1 15)) "context"
                                            ]
                                    )
                                    (Node (Range (Location 1 20) (Location 1 28)) "appState")
                            )
                        )
        , test "complex pattern" <|
            \() ->
                parseFullStringState emptyState "(Index irec as index, docVector)" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                TuplePattern
                                    [ Node empty <|
                                        AsPattern
                                            (Node empty <|
                                                NamedPattern (QualifiedNameRef [] "Index")
                                                    [ Node empty <| VarPattern "irec" ]
                                            )
                                            (Node empty "index")
                                    , Node empty <| VarPattern "docVector"
                                    ]
                            )
                        )
        , test "complex pattern 2" <|
            \() ->
                parseFullStringState emptyState "RBNode_elm_builtin col (RBNode_elm_builtin Red  (RBNode_elm_builtin Red xv))" Parser.pattern
                    |> Maybe.map noRangePattern
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                    [ Node empty <| VarPattern "col"
                                    , Node empty <|
                                        ParenthesizedPattern
                                            (Node empty <|
                                                NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                                    [ Node empty <| NamedPattern (QualifiedNameRef [] "Red") []
                                                    , Node empty <|
                                                        ParenthesizedPattern
                                                            (Node empty <|
                                                                NamedPattern (QualifiedNameRef [] "RBNode_elm_builtin")
                                                                    [ Node empty <| NamedPattern (QualifiedNameRef [] "Red") []
                                                                    , Node empty <| VarPattern "xv"
                                                                    ]
                                                            )
                                                    ]
                                            )
                                    ]
                            )
                        )
        ]
