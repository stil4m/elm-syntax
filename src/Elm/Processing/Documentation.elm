module Elm.Processing.Documentation exposing (postProcess)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
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
                { declarations = file.declarations
                , comments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = changes.declarations
    , comments = changes.comments
    }


type alias ThingsToChange =
    { declarations : List (Node Declaration)
    , comments : List (Node Comment)
    }


inspectDeclaration : Node Declaration -> ThingsToChange -> ThingsToChange
inspectDeclaration (Node r declaration) context =
    case declaration of
        FunctionDeclaration function ->
            onFunction (Node r function) context

        AliasDeclaration typeAlias ->
            onTypeAlias (Node r typeAlias) context

        CustomTypeDeclaration typeDecl ->
            onType (Node r typeDecl) context

        PortDeclaration _ ->
            context

        InfixDeclaration _ ->
            context

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            context


onType : Node Type -> ThingsToChange -> ThingsToChange
onType (Node r customType) file =
    case findDocumentationForRange r file.comments of
        Just ((Node docRange docString) as doc) ->
            { comments =
                file.comments
                    |> List.filter ((/=) doc)
            , declarations =
                List.map
                    (replaceDeclaration
                        (Node r (CustomTypeDeclaration <| { customType | documentation = Just (Node docRange docString) }))
                    )
                    file.declarations
            }

        Nothing ->
            file


onTypeAlias : Node TypeAlias -> ThingsToChange -> ThingsToChange
onTypeAlias (Node r typeAlias) file =
    case findDocumentationForRange r file.comments of
        Just ((Node docRange docString) as doc) ->
            { comments =
                file.comments
                    |> List.filter ((/=) doc)
            , declarations =
                List.map
                    (replaceDeclaration
                        (Node r
                            (AliasDeclaration
                                { typeAlias
                                    | documentation =
                                        Just (Node docRange docString)
                                }
                            )
                        )
                    )
                    file.declarations
            }

        Nothing ->
            file


onFunction : Node Function -> ThingsToChange -> ThingsToChange
onFunction (Node functionRange function) file =
    case findDocumentationForRange functionRange file.comments of
        Just ((Node docRange docString) as doc) ->
            { comments =
                file.comments
                    |> List.filter ((/=) doc)
            , declarations =
                List.map
                    (replaceDeclaration
                        (Node functionRange (FunctionDeclaration { function | documentation = Just (Node docRange docString) }))
                    )
                    file.declarations
            }

        Nothing ->
            file


replaceDeclaration : Node Declaration -> Node Declaration -> Node Declaration
replaceDeclaration (Node r1 new) (Node r2 old) =
    Node r2
        (if r1 == r2 then
            new

         else
            old
        )


findDocumentationForRange : Range -> List (Node String) -> Maybe (Node String)
findDocumentationForRange range comments =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            if isDocumentationForRange range comment then
                Just comment

            else
                findDocumentationForRange range restOfComments


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
