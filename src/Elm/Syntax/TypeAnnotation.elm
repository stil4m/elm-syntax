module Elm.Syntax.TypeAnnotation
    exposing
        ( RecordDefinition
        , RecordField
        , TypeAnnotation(FunctionTypeAnnotation, GenericRecord, GenericType, Record, Tupled, Typed, Unit)
        )

{-| Type Annotation Syntax


# Types

@docs TypeAnnotation, RecordDefinition, RecordField

-}

import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Ranged exposing (Ranged)


{-| Union type for different type aliases
-}
type TypeAnnotation
    = GenericType String
    | Typed ModuleName String (List (Ranged TypeAnnotation))
    | Unit
    | Tupled (List (Ranged TypeAnnotation))
    | Record RecordDefinition
    | GenericRecord String RecordDefinition
    | FunctionTypeAnnotation (Ranged TypeAnnotation) (Ranged TypeAnnotation)


{-| List of fields for a record
-}
type alias RecordDefinition =
    List RecordField


{-| Single field of a record. A name and its type
-}
type alias RecordField =
    ( String, Ranged TypeAnnotation )
