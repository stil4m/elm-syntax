module Elm.Parser.LayoutTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "LayoutTests"
        [ test "should consume" <|
            \() ->
                parseFullStringState emptyState "" Layout.layout
                    |> Expect.equal Nothing
        , test "just whitespace" <|
            \() ->
                parseFullStringState emptyState " " Layout.layout
                    |> Expect.equal (Just ())
        , test "spaces followed by new line" <|
            \() ->
                parseFullStringState emptyState " \n" Layout.layout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 2" <|
            \() ->
                parseFullStringState emptyState "\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 3" <|
            \() ->
                parseFullStringState emptyState " \n " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 4" <|
            \() ->
                parseFullStringState emptyState " \n  " (pushIndent 1 Layout.layout)
                    |> Expect.equal (Just ())
        , test "newlines spaces and single line comments" <|
            \() ->
                parseFullStringState emptyState "\n\n      --time\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "layoutStrict" <|
            \() ->
                parseFullStringState emptyState " \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict multi line" <|
            \() ->
                parseFullStringState emptyState " \n      \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict too much" <|
            \() ->
                parseFullStringState emptyState " \n " Layout.layoutStrict
                    |> Expect.equal Nothing
        , test "layoutStrict with comments" <|
            \() ->
                parseFullStringState emptyState "-- foo\n  --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments 2" <|
            \() ->
                parseFullStringState emptyState "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict some" <|
            \() ->
                parseFullStringState emptyState "\n  \n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments multi empty line preceding" <|
            \() ->
                parseFullStringState emptyState "\n\n --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiple new lines" <|
            \() ->
                parseFullStringState emptyState "\n  \n    \n\n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiline comment plus trailing whitespace" <|
            \() ->
                parseFullStringState emptyState "\n{- some note -}    \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        ]
