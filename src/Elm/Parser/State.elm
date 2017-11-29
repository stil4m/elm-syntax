module Elm.Parser.State exposing (State, addComment, currentIndent, emptyState, getComments, popIndent, pushIndent)

import Elm.Syntax.Ranged exposing (Ranged)


type State
    = State
        { indents : List Int
        , comments : List (Ranged String)
        }


emptyState : State
emptyState =
    State
        { indents = []
        , comments = []
        }


currentIndent : State -> Int
currentIndent (State { indents }) =
    List.head indents |> Maybe.withDefault 0


popIndent : State -> State
popIndent (State s) =
    State { s | indents = List.drop 1 s.indents }


pushIndent : Int -> State -> State
pushIndent x (State s) =
    State { s | indents = x :: s.indents }


addComment : Ranged String -> State -> State
addComment pair (State s) =
    State { s | comments = pair :: s.comments }


getComments : State -> List (Ranged String)
getComments (State s) =
    s.comments
