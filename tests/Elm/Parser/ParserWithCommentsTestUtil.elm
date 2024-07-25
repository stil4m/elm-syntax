module Elm.Parser.ParserWithCommentsTestUtil exposing (expectAst, expectAstWithComments, expectInvalid, parse, parseWithState)

import Elm.Syntax.Node exposing (Node)
import Expect
import Parser as Core exposing ((|.), DeadEnd)
import ParserWithComments exposing (ParserWithComments)
import Rope


parseWithState : String -> ParserWithComments a -> Maybe { comments : List (Node String), syntax : a }
parseWithState s p =
    case Core.run (p |. Core.end) s of
        Err _ ->
            Nothing

        Ok commentsAndSyntax ->
            { syntax = commentsAndSyntax.syntax
            , comments = commentsAndSyntax.comments |> Rope.toList
            }
                |> Just


parse : String -> ParserWithComments a -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map .syntax


parseWithFailure : String -> ParserWithComments a -> Result (List DeadEnd) a
parseWithFailure s p =
    case Core.run (p |. Core.end) s of
        Err deadEnds ->
            Err deadEnds

        Ok commentsAndSyntax ->
            commentsAndSyntax.syntax |> Ok


expectAst : ParserWithComments a -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case Core.run (parser |. Core.end) source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected
                    , \() ->
                        actual.comments
                            |> Rope.toList
                            |> Expect.equalLists []
                            |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                    ]
                    ()


expectAstWithComments : ParserWithComments a -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case Core.run (parser |. Core.end) source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected.ast
                    , \() -> actual.comments |> Rope.toList |> Expect.equal expected.comments
                    ]
                    ()


expectInvalid : ParserWithComments a -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
