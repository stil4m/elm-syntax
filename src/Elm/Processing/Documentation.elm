module Elm.Processing.Documentation exposing (postProcess)

import Elm.Inspector as Inspector exposing (Order(..), defaultConfig)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File -> File
postProcess file =
    Inspector.inspect
        { defaultConfig
            | onFunction = Post onFunction
            , onTypeAlias = Post onTypeAlias
            , onType = Post onType
        }
        file
        file


onType : Node Type -> File -> File
onType (Node r customType) file =
    case findDocumentationForRange r file.comments of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
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


onTypeAlias : Node TypeAlias -> File -> File
onTypeAlias (Node r typeAlias) file =
    case findDocumentationForRange r file.comments of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
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


onFunction : Node Function -> File -> File
onFunction (Node functionRange function) file =
    case findDocumentationForRange functionRange file.comments of
        Just ((Node docRange docString) as doc) ->
            { file
                | comments =
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
