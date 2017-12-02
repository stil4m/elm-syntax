module Elm.Processing.Documentation exposing (postProcess)

import Elm.Inspector as Inspector exposing (Order(Post), defaultConfig)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


postProcess : File -> File
postProcess file =
    Inspector.inspect
        { defaultConfig
            | onFunction = Post onFunction
            , onTypeAlias = Post onTypeAlias
        }
        file
        file


onTypeAlias : Ranged TypeAlias -> File -> File
onTypeAlias ( r, typeAlias ) file =
    let
        docs =
            List.filter (isDocumentationForRange r) file.comments
    in
    case List.head docs of
        Just (( docRange, docString ) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclaration
                            ( r
                            , AliasDecl
                                { typeAlias
                                    | documentation =
                                        Just (Documentation docString docRange)
                                }
                            )
                        )
                        file.declarations
            }

        Nothing ->
            file


onFunction : Ranged Function -> File -> File
onFunction ( functionRange, function ) file =
    let
        docs =
            List.filter (isDocumentationForRange functionRange) file.comments
    in
    case List.head docs of
        Just (( docRange, docString ) as doc) ->
            { file
                | comments =
                    file.comments
                        |> List.filter ((/=) doc)
                , declarations =
                    List.map
                        (replaceDeclaration
                            ( functionRange, FuncDecl { function | documentation = Just (Documentation docString docRange) } )
                        )
                        file.declarations
            }

        Nothing ->
            file


replaceDeclaration : Ranged Declaration -> Ranged Declaration -> Ranged Declaration
replaceDeclaration ( r1, new ) ( r2, old ) =
    ( r2
    , if r1 == r2 then
        new
      else
        old
    )


isDocumentationForRange : Range -> Ranged String -> Bool
isDocumentationForRange range ( commentRange, commentText ) =
    if String.startsWith "{-|" commentText then
        let
            functionStartRow =
                range.start.row
        in
        (commentRange.end.row + 1) == functionStartRow
    else
        False
