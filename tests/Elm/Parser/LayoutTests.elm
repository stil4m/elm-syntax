module Elm.Parser.LayoutTests exposing (all)

import Combine exposing (Parser)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (State)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
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
        , test "layout with multiline comment" <|
            \() ->
                parse "\n--x\n{- foo \n-}\n " Layout.layout
                    |> Expect.equal (Just ())
        , test "layout with documentation comment fails" <|
            \() ->
                parse "\n--x\n{-| foo \n-}\n " Layout.layout
                    |> Expect.equal Nothing
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
                parse "\n--x\n{- foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal (Just ())
        , test "layoutStrict with documentation comment fails" <|
            \() ->
                parse "\n--x\n{-| foo \n-}\n" Layout.layoutStrict
                    |> Expect.equal Nothing
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
        , test "declarationDocumentation when there is one" <|
            \() ->
                parse "{-| docs -}\n" Layout.declarationDocumentation
                    |> Expect.equal (Just (Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{-| docs -}")))
        , test "declarationDocumentation when there one documentation in the state not claimed by an import or declaration" <|
            \() ->
                parse ""
                    (modifyState
                        (Elm.Parser.State.addComment (Node Range.empty "{-| docs -}"))
                        Layout.declarationDocumentation
                    )
                    |> Expect.equal (Just (Just (Node Range.empty "{-| docs -}")))
        , test "declarationDocumentation when there one documentation in the state claimed by an import or declaration" <|
            \() ->
                parse ""
                    (modifyState
                        (Elm.Parser.State.parsedImportOrDeclaration
                            >> Elm.Parser.State.addComment (Node Range.empty "{-| docs -}")
                        )
                        Layout.declarationDocumentation
                    )
                    |> Expect.equal (Just Nothing)
        , test "declarationDocumentation when there is no documentation in the state" <|
            \() ->
                parse "" Layout.declarationDocumentation
                    |> Expect.equal (Just Nothing)
        ]


pushIndent : Int -> Parser State b -> Parser State b
pushIndent x p =
    Combine.modifyState (Elm.Parser.State.pushIndent (x + 1))
        |> Combine.continueWith p


modifyState : (State -> State) -> Parser State b -> Parser State b
modifyState f p =
    Combine.modifyState f |> Combine.continueWith p
