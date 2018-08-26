module Elm.Parser.State exposing (State, addComment, currentIndent, emptyState, expectedColumn, getComments, popIndent, pushColumn, storedColumns)

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


storedColumns : State -> List Int
storedColumns (State { indents }) =
    indents |> List.map ((+) 1)


expectedColumn : State -> Int
expectedColumn =
    currentIndent >> (+) 1


popIndent : State -> State
popIndent (State s) =
    State { s | indents = List.drop 1 s.indents }


pushIndent : Int -> State -> State
pushIndent x (State s) =
    State { s | indents = x :: s.indents }


pushColumn : Int -> State -> State
pushColumn col state =
    pushIndent (col - 1) state


addComment : Ranged String -> State -> State
addComment pair (State s) =
    State { s | comments = pair :: s.comments }


getComments : State -> List (Ranged String)
getComments (State s) =
    s.comments
