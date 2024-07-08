module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser, many, maybe, string)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


typeDefinition : Parser State (Node Declaration.Declaration)
typeDefinition =
    typePrefix
        |> Combine.andThen
            (\( start, maybeDocs ) ->
                Combine.oneOf
                    [ Combine.succeed
                        (\name generics typeAnnotation ->
                            Node
                                { start = start, end = (Node.range typeAnnotation).end }
                                (Declaration.AliasDeclaration
                                    { documentation = maybeDocs
                                    , name = name
                                    , generics = generics
                                    , typeAnnotation = typeAnnotation
                                    }
                                )
                        )
                        |> Combine.ignore (string "alias")
                        |> Combine.ignore Layout.layout
                        |> Combine.keep (Node.parser typeName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (string "=")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep typeAnnotation
                    , Combine.succeed
                        (\name generics constructors ->
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
                                { start = start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = maybeDocs
                                    , name = name
                                    , generics = generics
                                    , constructors = List.reverse constructors
                                    }
                                )
                        )
                        |> Combine.keep (Node.parser typeName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep genericList
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string "=")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep valueConstructors
                    ]
            )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1WithoutReverse
        (Combine.ignore (maybe Layout.layout) (string "|"))
        valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    Node.parser typeName
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    complete args =
                        let
                            endRange : Range
                            endRange =
                                List.head args |> Maybe.map Node.range |> Maybe.withDefault range
                        in
                        Combine.succeed
                            (Node
                                { start = range.start, end = endRange.end }
                                (ValueConstructor tnn (List.reverse args))
                            )

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
                                |> Combine.andThen complete
                            ]
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() -> argHelper [])
            )


genericList : Parser State (List (Node String))
genericList =
    many (Node.parser functionName |> Combine.ignore (maybe Layout.layout))


typePrefix : Parser State ( Location, Maybe (Node Documentation) )
typePrefix =
    Combine.succeed
        (\maybeDocs prefix ->
            case maybeDocs of
                Nothing ->
                    ( (Node.range prefix).start, Nothing )

                Just (Node { start } _) ->
                    ( start, maybeDocs )
        )
        |> Combine.keep Layout.declarationDocumentation
        |> Combine.keep (Node.parser (Combine.string "type"))
        |> Combine.ignore Layout.layout
