module Elm.Parser.Typings exposing (TypeDefinition(..), typeDefinition)

import Combine exposing (Parser, many, maybe, string, succeed)
import Combine.Extra as Combine
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged, withCurrentPoint)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
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
                            |> Combine.andMap (typeName |> Combine.ignore Layout.layout)
                            |> Combine.andMap genericList
                            |> Combine.ignore (string "=")
                            |> Combine.ignore Layout.layout
                            |> Combine.andMap typeAnnotation
                            |> Combine.map
                                (\typeAlias ->
                                    DefinedAlias (Range.combine [ start, Tuple.first typeAlias.typeAnnotation ]) typeAlias
                                )
                        , succeed Type
                            |> Combine.andMap typeName
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andMap genericList
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.ignore (string "=" |> Combine.ignore (maybe Layout.layout))
                            |> Combine.andMap valueConstructors
                            |> Combine.map
                                (\tipe ->
                                    DefinedType (Range.combine (start :: List.map .range tipe.constructors)) tipe
                                )
                        ]
                    )
        )


valueConstructors : Parser State (List ValueConstructor)
valueConstructors =
    Combine.lazy
        (\() ->
            Combine.succeed (::)
                |> Combine.andMap valueConstructor
                |> Combine.andMap
                    (Combine.choice
                        [ string "|"
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith valueConstructors
                        , Combine.succeed []
                        ]
                    )
        )


valueConstructor : Parser State ValueConstructor
valueConstructor =
    succeed ValueConstructor
        |> Combine.continueWith (ranged typeName)
        |> Combine.andThen
            (\( range, tn ) ->
                let
                    complete : List (Ranged TypeAnnotation) -> Parser State ValueConstructor
                    complete args =
                        Combine.succeed
                            (ValueConstructor tn args (Range.combine (range :: List.map Tuple.first args)))

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


genericList : Parser State (List String)
genericList =
    many (functionName |> Combine.ignore Layout.layout)


typePrefix : Parser State ()
typePrefix =
    Combine.string "type" |> Combine.continueWith Layout.layout
