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
                parse "     " (Whitespace.nSpaces 5)
                    |> Expect.equal (Just "     ")
        , test "nSpaces - not enough" <|
            \() ->
                parse "  " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "nSpaces - too much" <|
            \() ->
                parse "        " (Whitespace.nSpaces 5)
                    |> Expect.equal Nothing
        , test "manySpaces - empty" <|
            \() ->
                parse "" Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "manySpaces - not empty" <|
            \() ->
                parse "   " Whitespace.manySpaces
                    |> Expect.equal (Just ())
        , test "many1Spaces - empty" <|
            \() ->
                parse "" Whitespace.many1Spaces
                    |> Expect.equal Nothing
        , test "many1Spaces - not empty" <|
            \() ->
                parse "   " Whitespace.many1Spaces
                    |> Expect.equal (Just ())
        , test "realNewLine - normal" <|
            \() ->
                parse "\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\n")
        , test "realNewLine - with line feed" <|
            \() ->
                parse "\u{000D}\n" Whitespace.realNewLine
                    |> Expect.equal (Just "\u{000D}\n")
        , test "realNewLine - incorrect" <|
            \() ->
                parse "foo" Whitespace.realNewLine
                    |> Expect.equal Nothing
        ]
