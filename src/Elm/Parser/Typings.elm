module Elm.Parser.Typings exposing (TypeDefinition(..), typeDefinition)

import Combine exposing (Parser, many, maybe, string, succeed)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withCurrentPoint)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


type TypeDefinition
    = DefinedType Range Type
    | DefinedAlias Range TypeAlias


typeDefinition : Parser State TypeDefinition
typeDefinition =
    withCurrentPoint
        (\start ->
            typePrefix
                |> Combine.continueWith
                    (Combine.choice
                        [ succeed (TypeAlias Nothing)
                            |> Combine.ignore (string "alias" |> Combine.continueWith Layout.layout)
                            |> Combine.andMap (Node.parser typeName |> Combine.ignore (maybe Layout.layout))
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andMap genericList
                            |> Combine.ignore (string "=")
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andMap typeAnnotation
                            |> Combine.map
                                (\typeAlias ->
                                    DefinedAlias (Range.combine [ start, Node.range typeAlias.typeAnnotation ]) typeAlias
                                )
                        , succeed (Type Nothing)
                            |> Combine.andMap (Node.parser typeName)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andMap genericList
                            |> Combine.ignore (string "=")
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andThen (\tipe -> Combine.map (\( head, rest ) -> tipe head rest) valueConstructors)
                            |> Combine.map
                                (\tipe ->
                                    DefinedType
                                        (Range.combine
                                            (start
                                                :: List.map (\(Node r _) -> r)
                                                    (tipe.firstConstructor :: tipe.restOfConstructors)
                                            )
                                        )
                                        tipe
                                )
                        ]
                    )
        )


valueConstructors : Parser State ( Node Range ValueConstructor, List (Node Range ValueConstructor) )
valueConstructors =
    Combine.sepBy1 (Combine.ignore (maybe Layout.layout) (string "|")) valueConstructor


valueConstructor : Parser State (Node Range ValueConstructor)
valueConstructor =
    succeed ValueConstructor
        |> Combine.continueWith (Node.parser typeName)
        |> Combine.andThen
            (\((Node range _) as tnn) ->
                let
                    complete : List (Node Range TypeAnnotation) -> Parser State (Node Range ValueConstructor)
                    complete args =
                        Combine.succeed (Node (Range.combine (range :: List.map Node.range args)) (ValueConstructor tnn args))

                    argHelper : List (Node Range TypeAnnotation) -> Parser State (List (Node Range TypeAnnotation))
                    argHelper xs =
                        Combine.succeed ()
                            |> Combine.continueWith
                                (Combine.choice
                                    [ typeAnnotationNonGreedy
                                        |> Combine.andThen
                                            (\ta ->
                                                Layout.optimisticLayoutWith
                                                    (\() -> Combine.succeed (List.reverse (ta :: xs)))
                                                    (\() -> argHelper (ta :: xs))
                                            )
                                    , Combine.succeed (List.reverse xs)
                                    ]
                                )
                in
                Layout.optimisticLayoutWith
                    (\() -> complete [])
                    (\() ->
                        argHelper []
                            |> Combine.andThen complete
                    )
            )


genericList : Parser State (List (Node Range String))
genericList =
    many (Node.parser functionName |> Combine.ignore (maybe Layout.layout))


typePrefix : Parser State ()
typePrefix =
    Combine.string "type" |> Combine.continueWith Layout.layout
