module Elm.Parser.CommentTest exposing (all)

import Elm.Parser.Comments as Parser
import Elm.Parser.Node as Node
import Elm.Parser.ParserWithCommentsTestUtil exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Parser as Core exposing ((|.))
import Test exposing (..)


all : Test
all =
    describe "CommentTests"
        [ test "singleLineComment" <|
            \() ->
                parseSingleLineComment "--bar"
                    |> Expect.equal
                        (Ok (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
        , test "singleLineComment state" <|
            \() ->
                parseSingleLineComment "--bar"
                    |> Expect.equal
                        (Ok (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
        , test "singleLineComment does not include new line" <|
            \() ->
                parseSingleLineComment "--bar\n"
                    |> Expect.err
        , test "multilineComment parse result" <|
            \() ->
                parseMultiLineComment "{-foo\nbar-}"
                    |> Expect.equal
                        (Ok (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
        , test "multilineComment range" <|
            \() ->
                parseMultiLineComment "{-foo\nbar-}"
                    |> Expect.equal
                        (Ok (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
        , test "nested multilineComment only open" <|
            \() ->
                parseMultiLineComment "{- {- -}"
                    |> Expect.err
        , test "nested multilineComment open and close" <|
            \() ->
                parseMultiLineComment "{- {- -} -}"
                    |> Expect.equal
                        (Ok (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
        , test "multilineComment on module documentation" <|
            \() ->
                parseMultiLineComment "{-|foo\nbar-}"
                    |> Expect.err
        , test "module documentation" <|
            \() ->
                parseWithState "{-|foo\nbar-}" Parser.moduleDocumentation
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
        , test "module documentation can handle nested comments" <|
            \() ->
                parseWithState "{-| {- hello -} -}" Parser.moduleDocumentation
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}" ])
        ]


parseSingleLineComment : String -> Result (List Core.DeadEnd) (Node String)
parseSingleLineComment source =
    Core.run
        ((Parser.singleLineCommentCore |> Core.getChompedString |> Node.parserCore)
            |. Core.end
        )
        source


parseMultiLineComment : String -> Result (List Core.DeadEnd) (Node String)
parseMultiLineComment source =
    Core.run
        ((Parser.multilineCommentString |> Node.parserCore)
            |. Core.end
        )
        source
