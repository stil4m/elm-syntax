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
                parse "" Layout.layout
                    |> Expect.equal Nothing
        , test "just whitespace" <|
            \() ->
                parse " " Layout.layout
                    |> Expect.equal (Just ())
        , test "spaces followed by new line" <|
            \() ->
                parse " \n" Layout.layout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 2" <|
            \() ->
                parse "\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 3" <|
            \() ->
                parse " \n " Layout.layout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 4" <|
            \() ->
                parse " \n  " (pushIndent 1 Layout.layout)
                    |> Expect.equal (Just ())
        , test "newlines spaces and single line comments" <|
            \() ->
                parse "\n\n      --time\n  " Layout.layout
                    |> Expect.equal (Just ())
        , test "layoutStrict" <|
            \() ->
                parse " \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict multi line" <|
            \() ->
                parse " \n      \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict too much" <|
            \() ->
                parse " \n " Layout.layoutStrict
                    |> Expect.equal Nothing
        , test "layoutStrict with comments" <|
            \() ->
                parse "-- foo\n  --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments 2" <|
            \() ->
                parse "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict some" <|
            \() ->
                parse "\n  \n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments multi empty line preceding" <|
            \() ->
                parse "\n\n --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiple new lines" <|
            \() ->
                parse "\n  \n    \n\n  " (pushIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiline comment plus trailing whitespace" <|
            \() ->
                parse "\n{- some note -}    \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        ]
