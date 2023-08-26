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
                parseFullString "" Layout.layout
                    |> Expect.equal Nothing
        , test "just whitespace" <|
            \() ->
                parseFullString " " Layout.layout
                    |> Expect.equal (Just ())
        , test "spaces followed by new line" <|
            \() ->
                parseFullString " \n" Layout.layout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 2" <|
            \() ->
                parseFullString "\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 3" <|
            \() ->
                parseFullString " \n " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 4" <|
            \() ->
                parseFullString " \n  " (pushIndent 1 Layout.layout)
                    |> Expect.equal (Just ())
        , test "newlines spaces and single line comments" <|
            \() ->
                parseFullString "\n\n      --time\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "layoutStrict" <|
            \() ->
                parseFullString " \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict multi line" <|
            \() ->
                parseFullString " \n      \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict too much" <|
            \() ->
                parseFullString " \n " Layout.layoutStrict
                    |> Expect.equal Nothing
        , test "layoutStrict with comments" <|
            \() ->
                parseFullString "-- foo\n  --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments 2" <|
            \() ->
                parseFullString "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict some" <|
            \() ->
                parseFullString "\n  \n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments multi empty line preceding" <|
            \() ->
                parseFullString "\n\n --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiple new lines" <|
            \() ->
                parseFullString "\n  \n    \n\n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiline comment plus trailing whitespace" <|
            \() ->
                parseFullString "\n{- some note -}    \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        ]
