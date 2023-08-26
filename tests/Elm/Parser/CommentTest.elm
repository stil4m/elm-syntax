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
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ]
                            , indents = []
                            }
                        )
        , test "singleLineComment state" <|
            \() ->
                parseWithState "--bar" Parser.singleLineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ]
                            , indents = []
                            }
                        )
        , test "singleLineComment does not include new line" <|
            \() ->
                parse "--bar\n" Parser.singleLineComment
                    |> Expect.equal Nothing
        , test "multilineComment parse result" <|
            \() ->
                parseWithState "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ]
                            , indents = []
                            }
                        )
        , test "multilineComment range" <|
            \() ->
                parseWithState "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ]
                            , indents = []
                            }
                        )
        , test "nested multilineComment only open" <|
            \() ->
                parse "{- {- -}" Parser.multilineComment
                    |> Expect.equal Nothing
        , test "nested multilineComment open and close" <|
            \() ->
                parseWithState "{- {- -} -}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}" ]
                            , indents = []
                            }
                        )
        ]


toIndentAndComments : ( State, () ) -> { comments : List (Node String), indents : List Int }
toIndentAndComments ( state, () ) =
    { comments = State.getComments state
    , indents = State.storedColumns state
    }
