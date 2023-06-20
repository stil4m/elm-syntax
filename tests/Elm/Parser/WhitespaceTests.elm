module Elm.Parser.WhitespaceTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Whitespace as Whitespace
import Expect
import Test exposing (..)


all : Test
all =
    describe "WhitespaceLayoutTests"
        [ test "nSpaces - full" <|
            \() ->
                parseFullStringState emptyState "     " (Whitespace.nSpaces 5)
                    |> Expect.equal (Just "     ")
        , test "nSpaces - not enough" <|
            \() ->
                parseFullStringState emptyState "  " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "nSpaces - too much" <|
            \() ->
                parseFullStringState emptyState "        " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "manySpaces - empty" <|
            \() ->
                parseFullStringState emptyState "" Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "manySpaces - not empty" <|
            \() ->
                parseFullStringState emptyState "   " Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "many1Spaces - empty" <|
            \() ->
                parseFullStringState emptyState "" Whitespace.many1Spaces
                    |> Expect.equal Nothing
        , test "many1Spaces - not empty" <|
            \() ->
                parseFullStringState emptyState "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                parseFullStringState emptyState "\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\n")
        , test "realNewLine - with line feed" <|
            \() ->
                parseFullStringState emptyState "\u{000D}\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\u{000D}\n")
        , test "realNewLine - incorrect" <|
            \() ->
                parseFullStringState emptyState "foo" Whitespace.realNewLine
                    |> Expect.equal Nothing
        ]
