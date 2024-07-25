module Elm.Parser.State exposing
    ( State
    , addComment
    , combineWithIndent
    , currentIndent
    , emptyState
    , getComments
    , getCommentsFurthestToEarliest
    , setComments
    )

import Combine
import Elm.Syntax.Node exposing (Node)
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
