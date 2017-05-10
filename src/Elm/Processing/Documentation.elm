module Elm.Processing.Documentation exposing (postProcess)

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Documentation exposing (..)
import Elm.Inspector as Inspector exposing (Order(Post), defaultConfig)


postProcess : File -> File
postProcess file =
    Inspector.inspect
        { defaultConfig
            | onFunction = Post onFunction
            , onTypeAlias = Post onTypeAlias
        }
        file
        file


onTypeAlias : TypeAlias -> File -> File
onTypeAlias typeAlias file =
    let
        docs =
            List.filter (isDocumentationForRange typeAlias.range) file.comments
    in
        case List.head docs of
            Just (( docString, docRange ) as doc) ->
                { file
                    | comments =
                        file.comments
                            |> List.filter ((/=) doc)
                    , declarations =
                        List.map
                            (replaceTypeAlias
                                { typeAlias
                                    | documentation =
                                        Just (Documentation docString docRange)
                                }
                            )
                            file.declarations
                }

            Nothing ->
                file


onFunction : Function -> File -> File
onFunction function file =
    let
        functionRange =
            function.signature
                |> Maybe.map .range
                |> Maybe.withDefault function.declaration.name.range

        docs =
            List.filter (isDocumentationForRange functionRange) file.comments
    in
        case List.head docs of
            Just (( docString, docRange ) as doc) ->
                { file
                    | comments =
                        file.comments
                            |> List.filter ((/=) doc)
                    , declarations =
                        List.map
                            (replaceFunction
                                { function | documentation = Just (Documentation docString docRange) }
                            )
                            file.declarations
                }

            Nothing ->
                file


replaceTypeAlias : TypeAlias -> Declaration -> Declaration
replaceTypeAlias f1 decl =
    case decl of
        AliasDecl f2 ->
            if f1.range == f2.range then
                AliasDecl f1
            else
                decl

        _ ->
            decl


replaceFunction : Function -> Declaration -> Declaration
replaceFunction f1 decl =
    case decl of
        FuncDecl f2 ->
            if f1.declaration.name.range == f2.declaration.name.range then
                FuncDecl f1
            else
                decl

        _ ->
            decl


isDocumentationForRange : Range -> ( String, Range ) -> Bool
isDocumentationForRange range ( commentText, commentRange ) =
    if String.startsWith "{-|" commentText then
        let
            functionStartRow =
                range.start.row
        in
            commentRange.end.row == functionStartRow && commentRange.end.column == -2
    else
        False
