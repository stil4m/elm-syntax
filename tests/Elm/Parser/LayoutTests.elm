module Elm.Parser.LayoutTests exposing (all)

import CustomParser
import Elm.Parser.Layout as Layout
import Elm.Parser.ParserWithCommentsTestUtil exposing (parseWithState)
import Expect
import Parser exposing (Parser)
import ParserWithComments exposing (Comments)
import Test exposing (..)


all : Test
all =
    describe "LayoutTests"
        [ test "empty" <|
            \() ->
                parse "" (CustomParser.withIndent 0 Layout.maybeLayout)
                    |> Expect.equal (Just ())
        , test "just whitespace" <|
            \() ->
                parse " " Layout.maybeLayout
                    |> Expect.equal (Just ())
        , test "spaces followed by new line" <|
            \() ->
                parse " \n" Layout.maybeLayout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 2" <|
            \() ->
                parse "\n  " Layout.maybeLayout
                    |> Expect.equal (Just ())
        , test "with newline and higher indent 3" <|
            \() ->
                parse " \n " Layout.maybeLayout
                    |> Expect.equal (Just ())
        , test "maybeLayout with multiline comment" <|
            \() ->
                parse "\n--x\n{- foo \n-}\n " Layout.maybeLayout
                    |> Expect.equal (Just ())
        , test "maybeLayout with documentation comment fails" <|
            \() ->
                parse "\n--x\n{-| foo \n-}\n " Layout.maybeLayout
                    |> Expect.equal Nothing
        , test "with newline and higher indent 4" <|
            \() ->
                parse " \n  " (setIndent 1 Layout.maybeLayout)
                    |> Expect.equal (Just ())
        , test "newlines spaces and single line comments" <|
            \() ->
                parse "\n\n      --time\n  " Layout.maybeLayout
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
                parse "\n--x\n{- foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with documentation comment fails" <|
            \() ->
                parse "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal Nothing
        , test "layoutStrict some" <|
            \() ->
                parse "\n  \n  " (setIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with comments multi empty line preceding" <|
            \() ->
                parse "\n\n --bar\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiple new lines" <|
            \() ->
                parse "\n  \n    \n\n  " (setIndent 2 Layout.layoutStrict)
                    |> Expect.equal (Just ())
        , test "layoutStrict with multiline comment plus trailing whitespace" <|
            \() ->
                parse "\n{- some note -}    \n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        ]


setIndent : Int -> CustomParser.Parser a -> CustomParser.Parser a
setIndent x p =
    CustomParser.withIndent (x + 1)
        p


parse : String -> CustomParser.Parser Comments -> Maybe ()
parse source parser =
    parseWithState source
        (parser |> CustomParser.map (\c -> { comments = c, syntax = () }))
        |> Maybe.map .syntax
