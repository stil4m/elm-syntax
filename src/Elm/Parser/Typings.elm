module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser, many, maybe, string, succeed)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


typeDefinition : Parser State (Node Declaration.Declaration)
typeDefinition =
    Combine.withLocation
        (\start ->
            typePrefix
                |> Combine.continueWith
                    (Combine.oneOf
                        [ Combine.succeed
                            (\name generics typeAnnotation ->
                                Node
                                    { start = start, end = (Node.range typeAnnotation).end }
                                    (Declaration.AliasDeclaration
                                        { documentation = Nothing
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
                                -- Get the position from the last argument
                                Node
                                    (Range.combine ({ start = start, end = start } :: List.map Node.range constructors))
                                    (Declaration.CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = name
                                        , generics = generics
                                        , constructors = constructors
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
        )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1
        (Combine.ignore (maybe Layout.layout) (string "|"))
        valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    succeed ValueConstructor
        |> Combine.continueWith (Node.parser typeName)
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    complete args =
                        Combine.succeed
                            (Node
                                (Range.combine (range :: List.map Node.range args))
                                (ValueConstructor tnn (List.reverse args))
                            )

                    argHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
                    argHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNonGreedy
                                |> Combine.andThen
                                    (\ta ->
                                        Layout.optimisticLayoutWith
                                            (\() -> Combine.succeed (ta :: xs))
                                            (\() -> argHelper (ta :: xs))
                                    )
                            , Combine.succeed ()
                                |> Combine.map (\() -> xs)
                            ]
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() ->
                        argHelper []
                            |> Combine.andThen complete
                    )
            )


genericList : Parser State (List (Node String))
genericList =
    many (Node.parser functionName |> Combine.ignore (maybe Layout.layout))


typePrefix : Parser State ()
typePrefix =
    Combine.string "type" |> Combine.continueWith Layout.layout
