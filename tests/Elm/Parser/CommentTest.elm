module Elm.Parser.CommentTest exposing (all)

import CustomParser
import Elm.Parser.Comments as Parser
import Elm.Parser.Node as Node
import Elm.Parser.ParserWithCommentsTestUtil exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Rope
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
                parseWithState "{-|foo\nbar-}" (Parser.moduleDocumentation |> CustomParser.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
        , test "module documentation can handle nested comments" <|
            \() ->
                parseWithState "{-| {- hello -} -}" (Parser.moduleDocumentation |> CustomParser.map (\c -> { comments = Just (Rope.one c), syntax = () }))
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| {- hello -} -}" ])
        ]


parseSingleLineComment : String -> Result (List CustomParser.DeadEnd) (Node String)
parseSingleLineComment source =
    CustomParser.run
        ((Parser.singleLineCommentCore |> CustomParser.getChompedString |> Node.parserCore)
            |> CustomParser.ignore CustomParser.end
        )
        source


parseMultiLineComment : String -> Result (List CustomParser.DeadEnd) (Node String)
parseMultiLineComment source =
    CustomParser.run
        ((Parser.multilineCommentString |> Node.parserCore)
            |> CustomParser.ignore CustomParser.end
        )
        source
