module Elm.Parser.TypeAnnotation exposing (typeAnnotation)

import Combine exposing ((*>), (<$>), (<*), (<*>), (>>=), Parser, between, choice, lazy, many, map, maybe, or, parens, sepBy, string, succeed, whitespace)
import Elm.Parser.Ranges exposing (ranged, withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.Util exposing (moreThanIndentWhitespace, trimmed)
import Elm.Parser.Whitespace exposing (realNewLine)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (..)


typeAnnotationNoFn : Parser State (Ranged TypeAnnotation)
typeAnnotationNoFn =
    lazy
        (\() ->
            ranged <|
                choice
                    [ parensTypeAnnotation
                    , typedTypeAnnotation
                    , recordTypeAnnotation
                    , genericRecordTypeAnnotation
                    , genericTypeAnnotation
                    ]
        )


typeAnnotation : Parser State (Ranged TypeAnnotation)
typeAnnotation =
    lazy
        (\() ->
            ranged <|
                typeAnnotationNoFn
                    >>= (\typeRef ->
                            or (FunctionTypeAnnotation typeRef <$> (trimmed (string "->") *> typeAnnotation))
                                (succeed (Tuple.second typeRef))
                        )
        )


parensTypeAnnotation : Parser State TypeAnnotation
parensTypeAnnotation =
    lazy
        (\() ->
            parens (maybe moreThanIndentWhitespace *> sepBy (string ",") (trimmed typeAnnotation))
                |> map asTypeAnnotation
        )


asTypeAnnotation : List (Ranged TypeAnnotation) -> TypeAnnotation
asTypeAnnotation x =
    case x of
        [] ->
            Unit

        [ ( r, item ) ] ->
            item

        xs ->
            Tupled xs


genericTypeAnnotation : Parser State TypeAnnotation
genericTypeAnnotation =
    lazy (\() -> GenericType <$> functionName)


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (trimmed recordFieldDefinition))


genericRecordTypeAnnotation : Parser State TypeAnnotation
genericRecordTypeAnnotation =
    lazy
        (\() ->
            between
                (string "{")
                (maybe realNewLine *> string "}")
                (succeed GenericRecord
                    <*> (maybe whitespace *> functionName)
                    <*> (maybe whitespace *> string "|" *> maybe whitespace *> recordFieldsTypeAnnotation)
                )
        )


recordTypeAnnotation : Parser State TypeAnnotation
recordTypeAnnotation =
    lazy
        (\() ->
            between
                (string "{")
                (maybe realNewLine *> string "}")
                (Record <$> recordFieldsTypeAnnotation)
        )


recordFieldDefinition : Parser State RecordField
recordFieldDefinition =
    lazy
        (\() ->
            succeed (,)
                <*> (maybe moreThanIndentWhitespace *> functionName)
                <*> (maybe moreThanIndentWhitespace *> string ":" *> maybe moreThanIndentWhitespace *> typeAnnotation)
        )


typedTypeAnnotation : Parser State TypeAnnotation
typedTypeAnnotation =
    lazy
        (\() ->
            succeed Typed
                <*> many (typeName <* string ".")
                <*> typeName
                <*> (Maybe.withDefault []
                        <$> maybe (moreThanIndentWhitespace *> sepBy moreThanIndentWhitespace typeAnnotationNoFn)
                    )
        )
