module Elm.Processing.Documentation exposing (postProcess)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File -> File
postProcess file =
    let
        changes : ThingsToChange
        changes =
            List.foldl
                findAndAddDocumentation
                { declarations = []
                , previousComments = []
                , remainingComments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = List.reverse changes.declarations
    , comments =
        (changes.remainingComments :: changes.previousComments)
            |> List.reverse
            |> List.concat
    }


type alias ThingsToChange =
    { declarations : List (Node Declaration)
    , previousComments : List (List (Node Comment))
    , remainingComments : List (Node Comment)
    }


findAndAddDocumentation : Node Declaration -> ThingsToChange -> ThingsToChange
findAndAddDocumentation declaration context =
    case Node.value declaration of
        FunctionDeclaration function ->
            addDocumentation
                (\doc -> FunctionDeclaration { function | documentation = Just doc })
                declaration
                context

        AliasDeclaration typeAlias ->
            addDocumentation
                (\doc -> AliasDeclaration { typeAlias | documentation = Just doc })
                declaration
                context

        CustomTypeDeclaration typeDecl ->
            addDocumentation
                (\doc -> CustomTypeDeclaration { typeDecl | documentation = Just doc })
                declaration
                context

        PortDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        InfixDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }


addDocumentation : (Node Comment -> Declaration) -> Node Declaration -> ThingsToChange -> ThingsToChange
addDocumentation howToUpdate declaration file =
    let
        ( previous, maybeDoc, remaining ) =
            findDocumentationForRange (Node.range declaration) file.remainingComments []
    in
    case maybeDoc of
        Just doc ->
            { previousComments = previous :: file.previousComments
            , remainingComments = remaining
            , declarations = Node (Node.range declaration) (howToUpdate doc) :: file.declarations
            }

        Nothing ->
            { previousComments = previous :: file.previousComments
            , remainingComments = remaining
            , declarations = declaration :: file.declarations
            }


findDocumentationForRange : Range -> List (Node String) -> List (Node String) -> ( List (Node String), Maybe (Node String), List (Node String) )
findDocumentationForRange range comments previousComments =
    case comments of
        [] ->
            ( previousComments, Nothing, [] )

        ((Node commentRange commentText) as comment) :: restOfComments ->
            -- Since both comments and declarations are in the order that they appear in the source code,
            -- all the comments we've evaluated until now don't need to be re-evaluated when
            -- trying the find the documentation for later declarations if the current comment is later than the current declaration.
            case compare (commentRange.end.row + 1) range.start.row of
                EQ ->
                    if String.startsWith "{-|" commentText then
                        ( previousComments, Just comment, restOfComments )

                    else
                        -- Aborting because the next comment can't match the next declaration
                        ( previousComments, Nothing, comment :: restOfComments )

                LT ->
                    findDocumentationForRange range restOfComments (comment :: previousComments)

                GT ->
                    -- Aborting because we went too far
                    ( previousComments, Nothing, comment :: restOfComments )
