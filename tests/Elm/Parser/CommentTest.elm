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
                parseStateToMaybe "--bar" Parser.singleLineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ]
                            , indents = []
                            }
                        )
        , test "singleLineComment state" <|
            \() ->
                parseStateToMaybe "--bar" Parser.singleLineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar" ]
                            , indents = []
                            }
                        )
        , test "singleLineComment does not include new line" <|
            \() ->
                parseFullStringWithNullState "--bar\n" Parser.singleLineComment
                    |> Expect.equal Nothing
        , test "multilineComment parse result" <|
            \() ->
                parseStateToMaybe "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ]
                            , indents = []
                            }
                        )
        , test "multilineComment range" <|
            \() ->
                parseStateToMaybe "{-foo\nbar-}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}" ]
                            , indents = []
                            }
                        )
        , test "nested multilineComment only open" <|
            \() ->
                parseFullStringWithNullState "{- {- -}" Parser.multilineComment
                    |> Expect.equal Nothing
        , test "nested multilineComment open and close" <|
            \() ->
                parseStateToMaybe "{- {- -} -}" Parser.multilineComment
                    |> Maybe.map toIndentAndComments
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}" ]
                            , indents = []
                            }
                        )
        ]


toIndentAndComments : ( (), State ) -> { comments : List (Node String), indents : List Int }
toIndentAndComments ( (), state ) =
    { comments = State.getComments state
    , indents = State.storedColumns state
    }
