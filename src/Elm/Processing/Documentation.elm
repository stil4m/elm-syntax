module Elm.Processing.Documentation exposing (postProcess)

import Elm.Inspector as Inspector exposing (Order(..), defaultConfig)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File Range -> File Range
postProcess file =
    Inspector.inspect
        { defaultConfig
            | onFunction = Post onFunction
            , onTypeAlias = Post onTypeAlias
            , onType = Post onType
            , onPortDeclaration = Post onPort
        }
        file
        file


onType : Node Range Type -> File Range -> File Range
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


onTypeAlias : Node Range TypeAlias -> File Range -> File Range
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


onPort : Node Range Port -> File Range -> File Range
onPort (Node portRange portDeclaration) file =
    case findDocumentationForRange portRange file.comments of
        Just ((Node docRange _) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclarationByRange portRange
                            (Node (Range.combine [ docRange, portRange ]) <|
                                PortDeclaration
                                    (Port
                                        (Just doc)
                                        portDeclaration.signature
                                    )
                            )
                        )
                        file.declarations
            }

        Nothing ->
            file


onFunction : Node Range (Function Range) -> File Range -> File Range
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


replaceDeclarationByRange : Range -> Node Range Declaration -> Node Range Declaration -> Node Range Declaration
replaceDeclarationByRange targetRange newNode ((Node oldRange _) as oldNode) =
    if targetRange == oldRange then
        newNode

    else
        oldNode


replaceDeclaration : Node Range Declaration -> Node Range Declaration -> Node Range Declaration
replaceDeclaration (Node r1 new) (Node r2 old) =
    Node r2
        (if r1 == r2 then
            new

         else
            old
        )


findDocumentationForRange : Range -> List (Node Range String) -> Maybe (Node Range String)
findDocumentationForRange range comments =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            if isDocumentationForRange range comment then
                Just comment

            else
                findDocumentationForRange range restOfComments


isDocumentationForRange : Range -> Node Range String -> Bool
isDocumentationForRange range (Node commentRange commentText) =
    if String.startsWith "{-|" commentText then
        let
            functionStartRow =
                range.start.row
        in
        (commentRange.end.row + 1) == functionStartRow

    else
        False
