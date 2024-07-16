module Elm.Parser.WhitespaceTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil
import Elm.Parser.TestUtil as TestUtil
import Elm.Parser.Whitespace as Whitespace
import Expect
import Test exposing (..)


all : Test
all =
    describe "LayoutTests"
        [ test "many1Spaces - not empty" <|
            \() ->
                CombineTestUtil.parse "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                TestUtil.parse "\n" Whitespace.realNewLine
                    |> Expect.equal (Just ())
        , test "realNewLine - with line feed" <|
            \() ->
                TestUtil.parse "\u{000D}\n" Whitespace.realNewLine
                    |> Expect.equal (Just ())
        , test "realNewLine - incorrect" <|
            \() ->
                TestUtil.parse "foo" Whitespace.realNewLine
                    |> Expect.equal Nothing
        ]
