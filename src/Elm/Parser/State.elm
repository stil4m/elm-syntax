module Elm.Parser.State exposing (State, addComment, currentIndent, emptyState, expectedColumn, getComments, popIndent, pushIndent)

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
            head + 1


pushIndent : Int -> State -> State
pushIndent col (State s) =
    State { s | indents = (col - 1) :: s.indents }


popIndent : State -> State
popIndent (State s) =
    State { s | indents = List.drop 1 s.indents }


addComment : Node String -> State -> State
addComment pair (State s) =
    State { s | comments = pair :: s.comments }


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
