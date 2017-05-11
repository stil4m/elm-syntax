module Elm.Syntax.TypeAnnotation
    exposing
        ( TypeAnnotation(GenericType, Typed, Unit, Tupled, Record, GenericRecord, FunctionTypeAnnotation)
        , RecordDefinition
        , RecordField
        )

{-| Type Annotation Syntax


# Types

@docs TypeAnnotation, RecordDefinition, RecordField

-}

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Base exposing (ModuleName)


{-| Union type for different type aliases
-}
type TypeAnnotation
    = GenericType String Range
    | Typed ModuleName String (List TypeAnnotation) Range
    | Unit Range
    | Tupled (List TypeAnnotation) Range
    | Record RecordDefinition Range
    | GenericRecord String RecordDefinition Range
    | FunctionTypeAnnotation TypeAnnotation TypeAnnotation Range


{-| List of fields for a record
-}
type alias RecordDefinition =
    List RecordField


{-| Single field of a record. A name and its type
-}
type alias RecordField =
    ( String, TypeAnnotation )
