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
    )

import Combine
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Parser as Core


type State
    = Comments (List (Node String))


emptyState : State
emptyState =
    Comments []


setComments : List (Node String) -> State -> State
setComments commentsFurthestToEarliest _ =
    Comments commentsFurthestToEarliest


addComment : Node String -> State -> State
addComment commentToAdd (Comments soFarComments) =
    Comments (commentToAdd :: soFarComments)


addCommentAccordingToRange : Node String -> State -> State
addCommentAccordingToRange commentToAdd (Comments soFarComments) =
    Comments (insertIntoListAccordingToRange commentToAdd soFarComments)


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
getCommentsFurthestToEarliest (Comments comments) =
    comments


getComments : State -> List (Node String)
getComments (Comments comments) =
    List.reverse comments


currentIndent : Core.Parser Int
currentIndent =
    Core.getIndent


{-| For a given Combine.Parser, take the current start column as indentation for the whole block
-}
combineWithIndent : Combine.Parser State a -> Combine.Parser State a
combineWithIndent (Combine.Parser pFromState) =
    Combine.Parser
        (\state ->
            Core.andThen (\column -> Core.withIndent column (pFromState state))
                Core.getCol
        )
