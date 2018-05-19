module Elm.Parser.TypeAnnotation exposing (typeAnnotation)

import Combine exposing (..)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.Whitespace exposing (realNewLine)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (..)


type Mode
    = Eager
    | Lazy


typeAnnotationNoFn : Mode -> Parser State (Ranged TypeAnnotation)
typeAnnotationNoFn mode =
    lazy
        (\() ->
            ranged <|
                choice
                    [ parensTypeAnnotation
                    , typedTypeAnnotation mode
                    , recordTypeAnnotation
                    , genericRecordTypeAnnotation
                    , genericTypeAnnotation
                    ]
        )


typeAnnotation : Parser State (Ranged TypeAnnotation)
typeAnnotation =
    lazy
        (\() ->
            typeAnnotationNoFn Eager
                |> Combine.andThen
                    (\typeRef ->
                        or (Combine.map (FunctionTypeAnnotation typeRef) (Layout.maybeAroundBothSides (string "->") |> Combine.continueWith typeAnnotation))
                            (succeed (Tuple.second typeRef))
                    )
                |> ranged
        )


parensTypeAnnotation : Parser State TypeAnnotation
parensTypeAnnotation =
    lazy
        (\() ->
            parens (maybe Layout.layout |> Combine.continueWith (sepBy (string ",") (Layout.maybeAroundBothSides typeAnnotation)))
                |> map asTypeAnnotation
        )


asTypeAnnotation : List (Ranged TypeAnnotation) -> TypeAnnotation
asTypeAnnotation x =
    case x of
        [] ->
            Unit

        [ ( _, item ) ] ->
            item

        xs ->
            Tupled xs


genericTypeAnnotation : Parser State TypeAnnotation
genericTypeAnnotation =
    lazy (\() -> Combine.map GenericType functionName)


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (Layout.maybeAroundBothSides recordFieldDefinition))


genericRecordTypeAnnotation : Parser State TypeAnnotation
genericRecordTypeAnnotation =
    lazy
        (\() ->
            between
                (string "{")
                (maybe realNewLine |> Combine.continueWith (string "}"))
                (succeed GenericRecord
                    |> Combine.andMap (maybe whitespace |> Combine.continueWith functionName)
                    |> Combine.andMap (maybe whitespace |> Combine.continueWith (string "|") |> Combine.continueWith (maybe whitespace) |> Combine.continueWith recordFieldsTypeAnnotation)
                )
        )


recordTypeAnnotation : Parser State TypeAnnotation
recordTypeAnnotation =
    lazy
        (\() ->
            between
                (string "{")
                (maybe realNewLine |> Combine.continueWith (string "}"))
                (Combine.map Record recordFieldsTypeAnnotation)
        )


recordFieldDefinition : Parser State RecordField
recordFieldDefinition =
    lazy
        (\() ->
            succeed (\a b -> ( a, b ))
                |> Combine.andMap (maybe Layout.layout |> Combine.continueWith functionName)
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string ":")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith typeAnnotation
                    )
        )


typedTypeAnnotation : Mode -> Parser State TypeAnnotation
typedTypeAnnotation mode =
    lazy
        (\() ->
            case mode of
                Eager ->
                    succeed Typed
                        |> Combine.andMap (many (typeName |> Combine.ignore (string ".")))
                        |> Combine.andMap typeName
                        |> Combine.andMap
                            (Combine.map (Maybe.withDefault [])
                                (maybe (maybe Layout.layout |> Combine.continueWith (sepBy Layout.layout typeAnnotationNoFn)))
                            )

                Lazy ->
                    succeed Typed
                        |> Combine.andMap (many (typeName |> Combine.ignore (string ".")))
                        |> Combine.andMap typeName
                        |> Combine.andMap (succeed [])
        )
