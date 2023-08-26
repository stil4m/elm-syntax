module Elm.Parser.CombineTestUtil exposing (parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseWithState, pushIndent)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)


pushIndent : Int -> Parser State b -> Parser State b
pushIndent x p =
    modifyState (Elm.Parser.State.pushColumn (x + 1)) |> Combine.continueWith p


parseFullStringState : String -> Parser State a -> Maybe a
parseFullStringState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseWithState : String -> Parser State a -> Maybe ( a, State )
parseWithState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
        Ok ( x, r ) ->
            Just ( r, x )

        _ ->
            Nothing


parseFullStringWithNullState : String -> Parser State a -> Maybe a
parseFullStringWithNullState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseFullString : String -> Parser () a -> Maybe a
parseFullString s p =
    case Combine.parse (p |> Combine.ignore Combine.end) s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossibleWithState : State -> String -> Parser State a -> Maybe a
parseAsFarAsPossibleWithState state s p =
    case Combine.runParser p state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing
