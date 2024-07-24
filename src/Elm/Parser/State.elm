module Elm.Parser.State exposing
    ( State
    , addComment
    , addComments
    , currentIndent
    , emptyState
    , expectedColumn
    , getComments
    , popIndent
    , pushIndent
    )

import Elm.Syntax.Node exposing (Node)


type State
    = State
        { indents : List Int
        , comments : List (Node String)
        }


emptyState : State
emptyState =
    State
        { indents = []
        , comments = []
        }


currentIndent : State -> Maybe Int
currentIndent (State { indents }) =
    List.head indents


expectedColumn : State -> Int
expectedColumn (State { indents }) =
    case indents of
        [] ->
            1

        head :: _ ->
            head


pushIndent : Int -> State -> State
pushIndent col (State s) =
    State { s | indents = col :: s.indents }


popIndent : State -> State
popIndent (State s) =
    State
        { s
            | indents =
                case s.indents of
                    [] ->
                        []

                    _ :: restIndents ->
                        restIndents
        }


addComments : List (Node String) -> State -> State
addComments commentsFurthestToEarliest (State s) =
    State { s | comments = commentsFurthestToEarliest ++ s.comments }


addComment : Node String -> State -> State
addComment commentToAdd (State s) =
    State { s | comments = commentToAdd :: s.comments }


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
