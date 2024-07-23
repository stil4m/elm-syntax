module Elm.Parser.Typings exposing (typeOrTypeAliasDefinitionAfterDocumentation, typeOrTypeAliasDefinitionWithoutDocumentation)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|.))


typeOrTypeAliasDefinitionAfterDocumentation : Parser State (Node Documentation -> Parser State (Node Declaration.Declaration))
typeOrTypeAliasDefinitionAfterDocumentation =
    Core.map
        (\() ->
            \with ->
                \((Node documentationRange _) as documentation) ->
                    Combine.succeed (with documentationRange.start (Just documentation))
        )
        (Core.symbol "type")
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep typeOrTypeAliasDefinitionWith


typeOrTypeAliasDefinitionWithoutDocumentation : Parser State (Node Declaration.Declaration)
typeOrTypeAliasDefinitionWithoutDocumentation =
    Core.map
        (\( row, column ) ->
            \with ->
                with { row = row, column = column } Nothing
        )
        Core.getPosition
        |. Core.symbol "type"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep typeOrTypeAliasDefinitionWith


typeOrTypeAliasDefinitionWith : Parser State (Location -> Maybe (Node String) -> Node Declaration.Declaration)
typeOrTypeAliasDefinitionWith =
    Combine.oneOf
        [ Core.map
            (\() ->
                \name ->
                    \generics ->
                        \((Node { end } _) as typeAnnotation) ->
                            \start documentation ->
                                Node { start = start, end = end }
                                    (Declaration.AliasDeclaration
                                        { documentation = documentation
                                        , name = name
                                        , generics = generics
                                        , typeAnnotation = typeAnnotation
                                        }
                                    )
            )
            Tokens.aliasToken
            |> Combine.fromCoreIgnore Layout.layout
            |> Combine.keep typeNameLayout
            |> Combine.keep genericListEquals
            |> Combine.keep typeAnnotation
        , Combine.map
            (\name ->
                \generics ->
                    \constructors ->
                        \start documentation ->
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
                            Node { start = start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = documentation
                                    , name = name
                                    , generics = generics
                                    , constructors = List.reverse constructors
                                    }
                                )
            )
            typeNameLayout
            |> Combine.keep genericListEquals
            |> Combine.keep valueConstructors
        ]


locationEmpty : Location
locationEmpty =
    { row = 0, column = 0 }


typeNameLayout : Parser State (Node String)
typeNameLayout =
    Node.parserCore Tokens.typeName
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)


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
    Tokens.typeName
        |> Node.parserCore
        |> Combine.fromCore
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
                            { name = tnn, arguments = List.reverse args }

                    valueConstructorInnerArgHelper : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    valueConstructorInnerArgHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNoFnExcludingTypedWithArguments
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
            |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        )
