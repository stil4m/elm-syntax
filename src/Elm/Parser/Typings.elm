module Elm.Parser.Typings exposing (customTypeDefinitionWithoutDocumentation, typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix, typeDefinitionAfterDocumentation)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core


typeDefinitionAfterDocumentation : Node Documentation -> Parser State (Node Declaration.Declaration)
typeDefinitionAfterDocumentation documentation =
    typePrefix
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed
                    (\name ->
                        \generics ->
                            \((Node { end } _) as typeAnnotation) ->
                                Node
                                    { start = (Node.range documentation).start
                                    , end = end
                                    }
                                    (Declaration.AliasDeclaration
                                        { documentation = Just documentation
                                        , name = name
                                        , generics = generics
                                        , typeAnnotation = typeAnnotation
                                        }
                                    )
                    )
                    |> Combine.ignoreEntirely Tokens.aliasToken
                    |> Combine.ignore Layout.layout
                    |> Combine.keep typeNameLayout
                    |> Combine.keep genericListEquals
                    |> Combine.keep typeAnnotation
                , Combine.succeed
                    (\name ->
                        \generics ->
                            \constructors ->
                                let
                                    end : Location
                                    end =
                                        case constructors of
                                            (Node range _) :: _ ->
                                                range.end

                                            [] ->
                                                (Node.range documentation).start
                                in
                                Node
                                    { start = (Node.range documentation).start
                                    , end = end
                                    }
                                    (Declaration.CustomTypeDeclaration
                                        { documentation = Just documentation
                                        , name = name
                                        , generics = generics
                                        , constructors = List.reverse constructors
                                        }
                                    )
                    )
                    |> Combine.keep typeNameLayout
                    |> Combine.keep genericListEquals
                    |> Combine.keep valueConstructors
                ]
            )


typePrefix : Parser State ()
typePrefix =
    Core.symbol "type"
        |> Combine.ignoreFromCore Layout.layout


typePrefixBacktrackable : Parser State ()
typePrefixBacktrackable =
    typePrefix |> Combine.backtrackable


typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix : Parser State (Node Declaration.Declaration)
typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \name ->
                \generics ->
                    \((Node { end } _) as typeAnnotation) ->
                        Node { start = { row = startRow, column = startColumn }, end = end }
                            (Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = name
                                , generics = generics
                                , typeAnnotation = typeAnnotation
                                }
                            )
        )
        |> Combine.keepFromCore Core.getPosition
        |> Combine.ignore typePrefixBacktrackable
        |> Combine.ignoreEntirely Tokens.aliasToken
        |> Combine.ignore Layout.layout
        |> Combine.keep typeNameLayout
        |> Combine.keep genericListEquals
        |> Combine.keep typeAnnotation


customTypeDefinitionWithoutDocumentation : Parser State (Node Declaration.Declaration)
customTypeDefinitionWithoutDocumentation =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \name ->
                \generics ->
                    \constructors ->
                        let
                            end : Location
                            end =
                                case constructors of
                                    (Node range _) :: _ ->
                                        range.end

                                    -- should not happen
                                    [] ->
                                        locationEmpty
                        in
                        Node { start = { row = startRow, column = startColumn }, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = name
                                , generics = generics
                                , constructors = List.reverse constructors
                                }
                            )
        )
        |> Combine.keepFromCore Core.getPosition
        |> Combine.ignore typePrefix
        |> Combine.keep typeNameLayout
        |> Combine.keep genericListEquals
        |> Combine.keep valueConstructors


locationEmpty : Location
locationEmpty =
    { row = 0, column = 0 }


typeNameLayout : Parser State (Node String)
typeNameLayout =
    Node.parserCore Tokens.typeName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)


genericListEquals : Parser State (List (Node String))
genericListEquals =
    genericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1WithoutReverse
        (Tokens.pipe
            |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        )
        valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    Node.parserFromCore Tokens.typeName
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node TypeAnnotation) -> Node ValueConstructor
                    complete args =
                        let
                            endRange : Range
                            endRange =
                                case args of
                                    (Node lastArgRange _) :: _ ->
                                        lastArgRange

                                    [] ->
                                        range
                        in
                        Node
                            { start = range.start, end = endRange.end }
                            (ValueConstructor tnn (List.reverse args))

                    valueConstructorInnerArgHelper : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    valueConstructorInnerArgHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNonGreedy
                                |> Combine.andThen
                                    (\ta ->
                                        Layout.optimisticLayoutWith
                                            (\() -> complete (ta :: xs))
                                            (\() -> valueConstructorInnerArgHelper (ta :: xs))
                                    )
                            , Combine.succeedLazy (\() -> complete xs)
                            ]
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() -> valueConstructorInnerArgHelper [])
            )


genericList : Parser State (List (Node String))
genericList =
    Combine.many
        (Node.parserCore Tokens.functionName
            |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        )
