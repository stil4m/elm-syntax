module Elm.Parser.WhitespaceTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Whitespace as Whitespace
import Expect
import Test exposing (..)


all : Test
all =
    describe "LayoutTests"
        [ test "many1Spaces - not empty" <|
            \() ->
                parse "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                parse "\n" (Combine.fromCore Whitespace.realNewLine)
                    |> Expect.equal (Just ())
        , test "realNewLine - with line feed" <|
            \() ->
                parse "\u{000D}\n" (Combine.fromCore Whitespace.realNewLine)
                    |> Expect.equal (Just ())
        , test "realNewLine - incorrect" <|
            \() ->
                parse "foo" (Combine.fromCore Whitespace.realNewLine)
                    |> Expect.equal Nothing
        ]
