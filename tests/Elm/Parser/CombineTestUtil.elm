module Elm.Parser.CombineTestUtil exposing (expectAst, expectInvalid, parse, parseWithFailure, parseWithState)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State, emptyState)
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
        case parseWithFailure source parser of
            Err error ->
                Expect.fail ("Expected the source to be parsed correctly:\n" ++ Debug.toString error)

            Ok actual ->
                actual
                    |> Expect.equal expected


expectInvalid : Parser State a -> String -> Expect.Expectation
expectInvalid parser =
    \source ->
        case parseWithFailure source parser of
            Err _ ->
                Expect.pass

            Ok actual ->
                Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
