module Elm.Parser.TypeAnnotation exposing (typeAnnotation)

import Combine exposing ((*>), (<$>), (<*), (<*>), (>>=), Parser, between, choice, lazy, many, map, maybe, or, parens, sepBy, string, succeed, whitespace)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.Util exposing (moreThanIndentWhitespace, trimmed)
import Elm.Parser.Whitespace exposing (realNewLine)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (..)


typeAnnotationNoFn : Parser State TypeAnnotation
typeAnnotationNoFn =
    lazy
        (\() ->
            withRange <|
                choice
                    [ parensTypeAnnotation
                    , typedTypeAnnotation
                    , recordTypeAnnotation
                    , genericRecordTypeAnnotation
                    , genericTypeAnnotation
                    ]
        )


typeAnnotation : Parser State TypeAnnotation
typeAnnotation =
    lazy
        (\() ->
            withRange <|
                typeAnnotationNoFn
                    >>= (\typeRef ->
                            or (FunctionTypeAnnotation typeRef <$> (trimmed (string "->") *> typeAnnotation))
                                (succeed (always typeRef))
                        )
        )


parensTypeAnnotation : Parser State (Range -> TypeAnnotation)
parensTypeAnnotation =
    lazy
        (\() ->
            parens (maybe moreThanIndentWhitespace *> sepBy (string ",") (trimmed typeAnnotation))
                |> map asTypeAnnotation
        )


asTypeAnnotation : List TypeAnnotation -> (Range -> TypeAnnotation)
asTypeAnnotation x =
    case x of
        [] ->
            Unit

        [ item ] ->
            always item

        xs ->
            Tupled xs


genericTypeAnnotation : Parser State (Range -> TypeAnnotation)
genericTypeAnnotation =
    lazy (\() -> GenericType <$> functionName)


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (trimmed recordFieldDefinition))


genericRecordTypeAnnotation : Parser State (Range -> TypeAnnotation)
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


recordTypeAnnotation : Parser State (Range -> TypeAnnotation)
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


typedTypeAnnotation : Parser State (Range -> TypeAnnotation)
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
