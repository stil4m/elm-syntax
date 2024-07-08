module Elm.Parser.State exposing
    ( State
    , addComment
    , checkParsedImportOrDeclaration
    , currentIndent
    , emptyState
    , expectedColumn
    , getComments
    , parsedImportOrDeclaration
    , popIndent
    , pushIndent
    , removeComment
    )

import Elm.Syntax.Node exposing (Node)
import List.Extra


type State
    = State
        { indents : List Int
        , comments : List (Node String)
        , importOrDeclarationParsed : Bool
        }


emptyState : State
emptyState =
    State
        { indents = []
        , comments = []
        , importOrDeclarationParsed = False
        }


parsedImportOrDeclaration : State -> State
parsedImportOrDeclaration (State state) =
    State { state | importOrDeclarationParsed = True }


checkParsedImportOrDeclaration : State -> Bool
checkParsedImportOrDeclaration (State { importOrDeclarationParsed }) =
    importOrDeclarationParsed


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


removeComment : Node String -> State -> State
removeComment comment (State ({ comments } as state)) =
    State { state | comments = List.Extra.remove comment comments }


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
