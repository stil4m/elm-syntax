module Elm.Parser.ParserWithCommentsTestUtil exposing (expectAst, expectAstWithComments, expectInvalid, parse, parseWithState)

import Elm.Syntax.Node exposing (Node)
import Expect
import Parser exposing ((|.), DeadEnd, Parser)
import ParserWithComments exposing (WithComments)
import Rope


parseWithState : String -> Parser (WithComments a) -> Maybe { comments : List (Node String), syntax : a }
parseWithState s p =
    case Parser.run (p |. Parser.end) s of
        Err _ ->
            Nothing

        Ok commentsAndSyntax ->
            { comments = commentsAndSyntax.comments |> Rope.toList
            , syntax = commentsAndSyntax.syntax
            }
                |> Just


parse : String -> Parser (WithComments a) -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map .syntax


parseWithFailure : String -> Parser (WithComments a) -> Result (List DeadEnd) a
parseWithFailure s p =
    case Parser.run (p |. Parser.end) s of
        Err deadEnds ->
            Err deadEnds

        Ok commentsAndSyntax ->
            commentsAndSyntax.syntax |> Ok


expectAst : Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case Parser.run (parser |. Parser.end) source of
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


expectAstWithComments : Parser (WithComments a) -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case Parser.run (parser |. Parser.end) source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected.ast
                    , \() -> actual.comments |> Rope.toList |> Expect.equal expected.comments
                    ]
                    ()


expectInvalid : Parser (WithComments a) -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
