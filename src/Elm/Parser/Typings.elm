module Elm.Parser.Typings exposing (typeDefinition)

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


typeDefinition : Maybe (Node Documentation) -> Parser State (Node Declaration.Declaration)
typeDefinition maybeDoc =
    let
        startParser : Parser state Location
        startParser =
            case maybeDoc of
                Just (Node { start } _) ->
                    Combine.succeed start

                Nothing ->
                    Combine.location
    in
    startParser
        |> Combine.ignore typePrefix
        |> Combine.andThen
            (\start ->
                Combine.oneOf
                    [ Combine.succeed
                        (\name ->
                            \generics ->
                                \((Node { end } _) as typeAnnotation) ->
                                    Node
                                        { start = start
                                        , end = end
                                        }
                                        (Declaration.AliasDeclaration
                                            { documentation = maybeDoc
                                            , name = name
                                            , generics = generics
                                            , typeAnnotation = typeAnnotation
                                            }
                                        )
                        )
                        |> Combine.ignoreEntirely (Core.symbol "alias")
                        |> Combine.ignore Layout.layout
                        |> Combine.keep (Node.parserFromCore Tokens.typeName)
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignoreEntirely (Core.symbol "=")
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep typeAnnotation
                    , Combine.succeed
                        (\name ->
                            \generics ->
                                \constructors ->
                                    let
                                        end : Location
                                        end =
                                            case List.head constructors of
                                                Just (Node range _) ->
                                                    range.end

                                                Nothing ->
                                                    start
                                    in
                                    Node
                                        { start = start
                                        , end = end
                                        }
                                        (Declaration.CustomTypeDeclaration
                                            { documentation = maybeDoc
                                            , name = name
                                            , generics = generics
                                            , constructors = List.reverse constructors
                                            }
                                        )
                        )
                        |> Combine.keep (Node.parserFromCore Tokens.typeName)
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.ignoreEntirely (Core.symbol "=")
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep valueConstructors
                    ]
            )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1WithoutReverse
        (Combine.ignore (Combine.maybeIgnore Layout.layout) (Combine.symbol "|"))
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
                                List.head args |> Maybe.map Node.range |> Maybe.withDefault range
                        in
                        Node
                            { start = range.start, end = endRange.end }
                            (ValueConstructor tnn (List.reverse args))

                    argHelper : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    argHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNonGreedy
                                |> Combine.andThen
                                    (\ta ->
                                        Layout.optimisticLayoutWith
                                            (\() -> complete (ta :: xs))
                                            (\() -> argHelper (ta :: xs))
                                    )
                            , Combine.succeed xs
                                |> Combine.map complete
                            ]
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() -> argHelper [])
            )


genericList : Parser State (List (Node String))
genericList =
    Combine.many (Node.parserFromCore Tokens.functionName |> Combine.ignore (Combine.maybeIgnore Layout.layout))


typePrefix : Parser State ()
typePrefix =
    Combine.symbol "type"
        |> Combine.ignore Layout.layout
