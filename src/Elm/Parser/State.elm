module Elm.Parser.State exposing
    ( State
    , addComment
    , addCommentAccordingToRange
    , combineWithIndent
    , currentIndent
    , emptyState
    , getComments
    , getCommentsFurthestToEarliest
    , setComments
    , setIndent_TEST_ONLY
    )

import Combine
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Parser as Core


type State
    = State
        { indent : Int
        , comments : List (Node String)
        }


emptyState : State
emptyState =
    State
        { indent = 1
        , comments = []
        }


currentIndent : State -> Int
currentIndent (State { indent }) =
    indent


setComments : List (Node String) -> State -> State
setComments commentsFurthestToEarliest (State s) =
    State { s | comments = commentsFurthestToEarliest }


addComment : Node String -> State -> State
addComment commentToAdd (State s) =
    State { s | comments = commentToAdd :: s.comments }


{-| Use combineWithIndent instead
-}
setIndent_TEST_ONLY : Int -> State -> State
setIndent_TEST_ONLY indent (State s) =
    State { s | indent = indent }


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


{-| For a given Combine.Parser, take the current start column as indentation for the whole block
-}
combineWithIndent : Combine.Parser State a -> Combine.Parser State a
combineWithIndent (Combine.Parser pFromState) =
    Combine.Parser
        (\(State originalState) ->
            Core.getCol
                |> Core.andThen
                    (\column ->
                        pFromState (State { originalState | indent = column })
                    )
                |> Core.map
                    (\( State finalState, pValue ) ->
                        ( State { finalState | indent = originalState.indent }
                        , pValue
                        )
                    )
        )
