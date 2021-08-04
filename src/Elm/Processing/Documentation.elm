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
                inspectDeclaration
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


inspectDeclaration : Node Declaration -> ThingsToChange -> ThingsToChange
inspectDeclaration declaration context =
    case Node.value declaration of
        FunctionDeclaration function ->
            onFunction declaration function context

        AliasDeclaration typeAlias ->
            onTypeAlias declaration typeAlias context

        CustomTypeDeclaration typeDecl ->
            onType declaration typeDecl context

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


onType : Node Declaration -> Type -> ThingsToChange -> ThingsToChange
onType declaration customType file =
    addDocumentation
        (\doc -> CustomTypeDeclaration { customType | documentation = Just doc })
        declaration
        file


onTypeAlias : Node Declaration -> TypeAlias -> ThingsToChange -> ThingsToChange
onTypeAlias declaration typeAlias file =
    addDocumentation
        (\doc -> AliasDeclaration { typeAlias | documentation = Just doc })
        declaration
        file


onFunction : Node Declaration -> Function -> ThingsToChange -> ThingsToChange
onFunction declaration function file =
    addDocumentation
        (\doc -> FunctionDeclaration { function | documentation = Just doc })
        declaration
        file


replaceDeclaration : Node Declaration -> Node Declaration -> Node Declaration
replaceDeclaration (Node r1 new) (Node r2 old) =
    Node r2
        (if r1 == r2 then
            new

         else
            old
        )


findDocumentationForRange : Range -> List (Node String) -> List (Node String) -> Maybe ( List (Node String), Node String, List (Node String) )
findDocumentationForRange range comments previousIgnored =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            if isDocumentationForRange range comment then
                Just ( previousIgnored, comment, restOfComments )

            else
                findDocumentationForRange range restOfComments (comment :: previousIgnored)


isDocumentationForRange : Range -> Node String -> Bool
isDocumentationForRange range (Node commentRange commentText) =
    if String.startsWith "{-|" commentText then
        let
            functionStartRow =
                range.start.row
        in
        (commentRange.end.row + 1) == functionStartRow

    else
        False
