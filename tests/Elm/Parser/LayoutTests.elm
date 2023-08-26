module Elm.Parser.LayoutTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Layout as Layout
import Expect
import Test exposing (..)


all : Test
all =
    describe "LayoutTests"
        [ test "should consume" <|
            \() ->
                parseFullStringState "" Layout.layout
                    |> Expect.equal Nothing
        , test "just whitespace" <|
            \() ->
                parseFullStringState " " Layout.layout
                    |> Expect.equal (Just ())
        , test "spaces followed by new line" <|
            \() ->
                parseFullStringState " \n" Layout.layout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 2" <|
            \() ->
                parseFullStringState "\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 3" <|
            \() ->
                parseFullStringState " \n " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 4" <|
            \() ->
                parseFullStringState " \n  " (pushIndent 1 Layout.layout)
                    |> Expect.equal (Just ())
        , test "newlines spaces and single line comments" <|
            \() ->
                parseFullStringState "\n\n      --time\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "layoutStrict" <|
            \() ->
                parseFullStringState " \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict multi line" <|
            \() ->
                parseFullStringState " \n      \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict too much" <|
            \() ->
                parseFullStringState " \n " Layout.layoutStrict
                    |> Expect.equal Nothing
        , test "layoutStrict with comments" <|
            \() ->
                parseFullStringState "-- foo\n  --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments 2" <|
            \() ->
                parseFullStringState "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict some" <|
            \() ->
                parseFullStringState "\n  \n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments multi empty line preceding" <|
            \() ->
                parseFullStringState "\n\n --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiple new lines" <|
            \() ->
                parseFullStringState "\n  \n    \n\n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiline comment plus trailing whitespace" <|
            \() ->
                parseFullStringState "\n{- some note -}    \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        ]
