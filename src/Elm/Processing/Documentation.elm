module Elm.Processing.Documentation exposing (postProcess)

import Elm.Inspector as Inspector exposing (Order(..), defaultConfig)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File -> File
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


onType : Node Type -> File -> File
onType (Node r customType) file =
    let
        docs : List (Node String)
        docs =
            List.filter (isDocumentationForRange r) file.comments
    in
    case List.head docs of
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
    let
        docs : List (Node String)
        docs =
            List.filter (isDocumentationForRange r) file.comments
    in
    case List.head docs of
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


onPort : Node Port -> File -> File
onPort (Node portRange portDeclaration) file =
    let
        docs : List (Node String)
        docs =
            List.filter (isDocumentationForRange portRange) file.comments
    in
    case List.head docs of
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


onFunction : Node Function -> File -> File
onFunction (Node functionRange function) file =
    let
        docs : List (Node String)
        docs =
            List.filter (isDocumentationForRange functionRange) file.comments
    in
    case List.head docs of
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


replaceDeclarationByRange : Range -> Node Declaration -> Node Declaration -> Node Declaration
replaceDeclarationByRange targetRange newNode ((Node oldRange _) as oldNode) =
    if targetRange == oldRange then
        newNode

    else
        oldNode


replaceDeclaration : Node Declaration -> Node Declaration -> Node Declaration
replaceDeclaration (Node r1 new) (Node r2 old) =
    Node r2
        (if r1 == r2 then
            new

         else
            old
        )


isDocumentationForRange : Range -> Node String -> Bool
isDocumentationForRange range (Node commentRange commentText) =
    if String.startsWith "{-|" commentText then
        let
            functionStartRow : Int
            functionStartRow =
                range.start.row
        in
        (commentRange.end.row + 1) == functionStartRow

    else
        False
