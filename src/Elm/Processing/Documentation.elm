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
                , unattachedComments = []
                , remainingComments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = List.reverse changes.declarations
    , comments =
        (changes.remainingComments :: changes.unattachedComments)
            |> List.reverse
            |> List.concat
    }


type alias ThingsToChange =
    { declarations : List (Node Declaration)
    , unattachedComments : List (List (Node Comment))
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
            { unattachedComments = context.unattachedComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        InfixDeclaration _ ->
            { unattachedComments = context.unattachedComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            { unattachedComments = context.unattachedComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }


addDocumentation : (Node Comment -> Declaration) -> Node Declaration -> ThingsToChange -> ThingsToChange
addDocumentation howToUpdate declaration file =
    case findDocumentationForRange (Node.range declaration) file.remainingComments [] of
        Just ( ignored, doc, remaining ) ->
            { unattachedComments = ignored :: file.unattachedComments
            , remainingComments = remaining
            , declarations = Node (Node.range declaration) (howToUpdate doc) :: file.declarations
            }

        Nothing ->
            { unattachedComments = file.unattachedComments
            , remainingComments = file.remainingComments
            , declarations = declaration :: file.declarations
            }


findDocumentationForRange : Range -> List (Node String) -> List (Node String) -> Maybe ( List (Node String), Node String, List (Node String) )
findDocumentationForRange range comments previousIgnored =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            if isDocumentationForRange range comment then
                -- Since both comments and declarations are in the order that they appear in the source code,
                -- all the comments we've evaluated until now don't need to be re-evaluated when
                -- trying the find the documentation for later declarations.
                Just ( previousIgnored, comment, restOfComments )

            else
                findDocumentationForRange range restOfComments (comment :: previousIgnored)


isDocumentationForRange : Range -> Node String -> Bool
isDocumentationForRange range (Node commentRange commentText) =
    if String.startsWith "{-|" commentText then
        (commentRange.end.row + 1) == range.start.row

    else
        False
