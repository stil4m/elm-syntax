module Elm.Parser.WhitespaceTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Whitespace as Whitespace
import Expect
import Test exposing (..)


all : Test
all =
    describe "LayoutTests"
        [ test "nSpaces - full" <|
            \() ->
                parseFullStringState "     " (Whitespace.nSpaces 5)
                    |> Expect.equal (Just "     ")
        , test "nSpaces - not enough" <|
            \() ->
                parseFullStringState "  " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "nSpaces - too much" <|
            \() ->
                parseFullStringState "        " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "manySpaces - empty" <|
            \() ->
                parseFullStringState "" Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "manySpaces - not empty" <|
            \() ->
                parseFullStringState "   " Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "many1Spaces - empty" <|
            \() ->
                parseFullStringState "" Whitespace.many1Spaces
                    |> Expect.equal Nothing
        , test "many1Spaces - not empty" <|
            \() ->
                parseFullStringState "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                parseFullStringState "\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\n")
        , test "realNewLine - with line feed" <|
            \() ->
                parseFullStringState "\u{000D}\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\u{000D}\n")
        , test "realNewLine - incorrect" <|
            \() ->
                parseFullStringState "foo" Whitespace.realNewLine
                    |> Expect.equal Nothing
        ]
