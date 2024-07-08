module Elm.Parser.CommentTest exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Comments as Parser
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "CommentTests"
        [ test "singleLineComment" <|
            \() ->
                parseWithState "--bar" Parser.singleLineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ])
        , test "singleLineComment state" <|
            \() ->
                parseWithState "--bar" Parser.singleLineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ])
        , test "singleLineComment does not include new line" <|
            \() ->
                parse "--bar\n" Parser.singleLineComment
                    |> Expect.equal Nothing
        , test "multilineComment parse result" <|
            \() ->
                parseWithState "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ])
        , test "multilineComment range" <|
            \() ->
                parseWithState "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ])
        , test "nested multilineComment only open" <|
            \() ->
                parse "{- {- -}" Parser.multilineComment
                    |> Expect.equal Nothing
        , test "nested multilineComment open and close" <|
            \() ->
                parseWithState "{- {- -} -}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}" ])
        , test "multilineComment on module documentation" <|
            \() ->
                parseWithState "{-|foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal Nothing
        , test "module documentation" <|
            \() ->
                parseWithState "{-|foo\nbar-}" Parser.moduleDocumentation
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-|foo\nbar-}" ])
        ]


toIndentAndComments : ( State, () ) -> List (Node String)
toIndentAndComments ( state, () ) =
    State.getComments state
