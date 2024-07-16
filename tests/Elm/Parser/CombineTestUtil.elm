module Elm.Parser.CombineTestUtil exposing (expectAst, expectAstWithComments, expectInvalid, parse, parseWithState)

import Combine exposing (Parser)
import Elm.Parser.State as State exposing (State, emptyState)
import Elm.Syntax.Node exposing (Node)
import Expect
import Parser exposing (DeadEnd)


parseWithState : String -> Parser State a -> Maybe ( State, a )
parseWithState s p =
    Combine.runParser (p |> Combine.ignore Combine.end) emptyState s
        |> Result.toMaybe


parse : String -> Parser State a -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map Tuple.second


parseWithFailure : String -> Parser State a -> Result (List DeadEnd) a
parseWithFailure s p =
    Combine.runParser (p |> Combine.ignore Combine.end) emptyState s
        |> Result.map Tuple.second


expectAst : Parser State a -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case Combine.runParser (parser |> Combine.ignore Combine.end) emptyState source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \( _, ast ) -> ast |> Expect.equal expected
                    , \( state, _ ) ->
                        State.getComments state
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    actual


expectAstWithComments : Parser State a -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case Combine.runParser (parser |> Combine.ignore Combine.end) emptyState source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \( _, ast ) -> ast |> Expect.equal expected.ast
                    , \( state, _ ) -> State.getComments state |> Expect.equal expected.comments
                    ]
                    actual


expectInvalid : Parser State a -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
