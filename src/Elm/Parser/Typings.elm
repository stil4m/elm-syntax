module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core
import Parser.Extra


typeDefinition : Maybe (Node Documentation) -> Parser State (Node Declaration.Declaration)
typeDefinition maybeDoc =
    let
        startParser : Core.Parser Location
        startParser =
            case maybeDoc of
                Just (Node { start } _) ->
                    Core.succeed start

                Nothing ->
                    Parser.Extra.location
    in
    startParser
        |> Combine.ignoreFromCore typePrefix
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
                        |> Combine.keepFromCore (Node.parserCore Tokens.typeName)
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignoreEntirely Tokens.equal
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
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
                        |> Combine.keepFromCore (Node.parserCore Tokens.typeName)
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.ignoreEntirely Tokens.equal
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        |> Combine.keep valueConstructors
                    ]
            )


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


typePrefix : Parser State ()
typePrefix =
    Core.symbol "type"
        |> Combine.ignoreFromCore Layout.layout
