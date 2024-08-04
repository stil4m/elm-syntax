module Elm.Parser.ParserWithCommentsTestUtil exposing (expectAst, expectAstWithComments, expectInvalid, parse, parseWithState)

import CustomParser
import Elm.Syntax.Node exposing (Node)
import Expect
import ParserWithComments exposing (WithComments)
import Rope


parseWithState : String -> CustomParser.Parser (WithComments a) -> Maybe { comments : List (Node String), syntax : a }
parseWithState s p =
    case CustomParser.run (p |> CustomParser.ignore CustomParser.end) s of
        Err _ ->
            Nothing

        Ok commentsAndSyntax ->
            { comments = commentsAndSyntax.comments |> Rope.toList
            , syntax = commentsAndSyntax.syntax
            }
                |> Just


parse : String -> CustomParser.Parser (WithComments a) -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map .syntax


parseWithFailure : String -> CustomParser.Parser (WithComments a) -> Result (List CustomParser.DeadEnd) a
parseWithFailure s p =
    case CustomParser.run (p |> CustomParser.ignore CustomParser.end) s of
        Err deadEnds ->
            Err deadEnds

        Ok commentsAndSyntax ->
            commentsAndSyntax.syntax |> Ok


expectAst : CustomParser.Parser (WithComments a) -> a -> String -> Expect.Expectation
expectAst parser =
    \expected source ->
        case CustomParser.run (parser |> CustomParser.ignore CustomParser.end) source of
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


expectAstWithComments : CustomParser.Parser (WithComments a) -> { ast : a, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments parser =
    \expected source ->
        case CustomParser.run (parser |> CustomParser.ignore CustomParser.end) source of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                Expect.all
                    [ \() -> actual.syntax |> Expect.equal expected.ast
                    , \() -> actual.comments |> Rope.toList |> Expect.equal expected.comments
                    ]
                    ()


expectInvalid : CustomParser.Parser (WithComments a) -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
