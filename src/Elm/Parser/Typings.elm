module Elm.Parser.Typings exposing (typeDefinition)

import Combine exposing (Parser, many, maybe, string, succeed)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withCurrentPoint)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


typeDefinition : Parser State (Node Declaration.Declaration)
typeDefinition =
    withCurrentPoint
        (\start ->
            typePrefix
                |> Combine.continueWith
                    (Combine.oneOf
                        [ succeed (TypeAlias Nothing)
                            |> Combine.ignore (string "alias" |> Combine.continueWith Layout.layout)
                            |> Combine.keep (Node.parser typeName |> Combine.ignore (maybe Layout.layout))
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.keep genericList
                            |> Combine.ignore (string "=")
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.keep typeAnnotation
                            |> Combine.map
                                (\typeAlias ->
                                    Node
                                        (Range.combine [ start, Node.range typeAlias.typeAnnotation ])
                                        (Declaration.AliasDeclaration typeAlias)
                                )
                        , succeed (Type Nothing)
                            |> Combine.keep (Node.parser typeName)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.keep genericList
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.ignore (string "=" |> Combine.ignore (maybe Layout.layout))
                            |> Combine.keep valueConstructors
                            |> Combine.map
                                (\tipe ->
                                    Node
                                        (Range.combine (start :: List.map Node.range tipe.constructors))
                                        (Declaration.CustomTypeDeclaration tipe)
                                )
                        ]
                    )
        )


valueConstructors : Parser State (List (Node ValueConstructor))
valueConstructors =
    Combine.sepBy1 (Combine.ignore (maybe Layout.layout) (string "|")) valueConstructor


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    succeed ValueConstructor
        |> Combine.continueWith (Node.parser typeName)
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node TypeAnnotation) -> Parser State (Node ValueConstructor)
                    complete args =
                        Combine.succeed (Node (Range.combine (range :: List.map Node.range args)) (ValueConstructor tnn args))

                    argHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
                    argHelper xs =
                        Combine.oneOf
                            [ typeAnnotationNonGreedy
                                |> Combine.andThen
                                    (\ta ->
                                        Layout.optimisticLayoutWith
                                            (\() -> Combine.succeed (List.reverse (ta :: xs)))
                                            (\() -> argHelper (ta :: xs))
                                    )
                            , Combine.succeed (List.reverse xs)
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
