module Elm.Parser.State exposing
    ( State
    , addComment
    , addCommentAccordingToRange
    , currentIndent
    , emptyState
    , getComments
    , getCommentsFurthestToEarliest
    , popIndent
    , pushIndent
    , setComments
    )

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)


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


currentIndent : State -> Int
currentIndent (State { indents }) =
    case indents of
        [] ->
            1

        topIndent :: _ ->
            topIndent


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


setComments : List (Node String) -> State -> State
setComments commentsFurthestToEarliest (State s) =
    State { s | comments = commentsFurthestToEarliest }


addComment : Node String -> State -> State
addComment commentToAdd (State s) =
    State { s | comments = commentToAdd :: s.comments }


addCommentAccordingToRange : Node String -> State -> State
addCommentAccordingToRange commentToAdd (State s) =
    State { s | comments = insertIntoListAccordingToRange commentToAdd s.comments }


insertIntoListAccordingToRange : Node a -> List (Node a) -> List (Node a)
insertIntoListAccordingToRange ((Node toInsertRange _) as toInsert) list =
    case list of
        [] ->
            List.singleton toInsert

        ((Node headRange _) as head) :: tail ->
            if toInsertRange.start |> locationIsFurtherThen headRange.start then
                toInsert :: head :: tail

            else
                head :: insertIntoListAccordingToRange toInsert (toInsert :: tail)


locationIsFurtherThen : Location -> Location -> Bool
locationIsFurtherThen comparedAgainst base =
    case compare base.row comparedAgainst.row of
        GT ->
            True

        LT ->
            False

        EQ ->
            base.column > comparedAgainst.column


getCommentsFurthestToEarliest : State -> List (Node String)
getCommentsFurthestToEarliest (State s) =
    s.comments


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
