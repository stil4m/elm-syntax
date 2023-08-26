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
                parseFullString "     " (Whitespace.nSpaces 5)
                    |> Expect.equal (Just "     ")
        , test "nSpaces - not enough" <|
            \() ->
                parseFullString "  " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "nSpaces - too much" <|
            \() ->
                parseFullString "        " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "manySpaces - empty" <|
            \() ->
                parseFullString "" Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "manySpaces - not empty" <|
            \() ->
                parseFullString "   " Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "many1Spaces - empty" <|
            \() ->
                parseFullString "" Whitespace.many1Spaces
                    |> Expect.equal Nothing
        , test "many1Spaces - not empty" <|
            \() ->
                parseFullString "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                parseFullString "\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\n")
        , test "realNewLine - with line feed" <|
            \() ->
                parseFullString "\u{000D}\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\u{000D}\n")
        , test "realNewLine - incorrect" <|
            \() ->
                parseFullString "foo" Whitespace.realNewLine
                    |> Expect.equal Nothing
        ]
