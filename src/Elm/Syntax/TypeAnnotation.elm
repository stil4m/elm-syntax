module Elm.Syntax.TypeAnnotation exposing
    ( TypeAnnotation(..), RecordField
    , encode, decoder
    )

{-|


# Type Annotation Syntax

This syntax represents the type annotation syntax.
For example:

    Int -> String


## Types

@docs TypeAnnotation, RecordField


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Custom type for different type annotations. For example:

  - `Var`: `a`
  - `Type`: `Maybe (Int -> String)`
  - `Tuples`: `(a, b, c)` or Unit `()`
  - `Record`: `{ name : String }`
  - `ExtensionRecord`: `{ a | name : String }`
  - `GenericRecord`: `{ a | name : String}`
  - `FunctionTypeAnnotation`: `Int -> String`

-}
type TypeAnnotation
    = Var String
    | Type (Node Range ( ModuleName, String )) (List (Node Range TypeAnnotation))
    | Tuple (List (Node Range TypeAnnotation))
    | Record (List (Node Range RecordField))
    | ExtensionRecord (Node Range String) (Node Range RecordField) (List (Node Range RecordField))
    | FunctionTypeAnnotation (Node Range TypeAnnotation) (Node Range TypeAnnotation)


{-| Single field of a record. A name and its type.
-}
type alias RecordField =
    ( Node Range String, Node Range TypeAnnotation )



-- Serialization


{-| Encode a `TypeAnnotation` syntax element to JSON.
-}
encode : TypeAnnotation -> Value
encode typeAnnotation =
    case typeAnnotation of
        Var name ->
            encodeTyped "var" <|
                JE.object
                    [ ( "value", JE.string name )
                    ]

        Type moduleNameAndName args ->
            let
                inner : ( ModuleName, String ) -> Value
                inner ( mod, n ) =
                    JE.object
                        [ ( "moduleName", ModuleName.encode mod )
                        , ( "name", JE.string n )
                        ]
            in
            encodeTyped "type" <|
                JE.object
                    [ ( "moduleNameAndName", Node.encode inner moduleNameAndName )
                    , ( "args", JE.list (Node.encode encode) args )
                    ]

        Tuple t ->
            encodeTyped "tuple" <|
                JE.object
                    [ ( "values", JE.list (Node.encode encode) t )
                    ]

        FunctionTypeAnnotation left right ->
            encodeTyped "function" <|
                JE.object
                    [ ( "left", Node.encode encode left )
                    , ( "right", Node.encode encode right )
                    ]

        Record recordDefinition ->
            encodeTyped "record" <|
                JE.object
                    [ ( "value", JE.list (Node.encode encodeRecordField) recordDefinition )
                    ]

        ExtensionRecord generic firstField restOfFields ->
            encodeTyped "extension" <|
                JE.object
                    [ ( "generic", Node.encode JE.string generic )
                    , ( "firstField", Node.encode encodeRecordField firstField )
                    , ( "restOfFields", JE.list (Node.encode encodeRecordField) restOfFields )
                    ]


encodeRecordField : RecordField -> Value
encodeRecordField ( name, ref ) =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "typeAnnotation", Node.encode encode ref )
        ]


decodeModuleNameAndName : Decoder ( ModuleName, String )
decodeModuleNameAndName =
    JD.map2 Tuple.pair
        (JD.field "moduleName" <| ModuleName.decoder)
        (JD.field "name" <| JD.string)


{-| JSON decoder for a `TypeAnnotation` syntax element.
-}
decoder : Decoder TypeAnnotation
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "var", JD.map Var (JD.field "value" JD.string) )
                , ( "type"
                  , JD.map2 Type
                        (JD.field "moduleNameAndName" <| Node.decoder decodeModuleNameAndName)
                        (JD.field "args" (JD.list nestedDecoder))
                  )
                , ( "tuple", JD.map Tuple (JD.field "values" (JD.list nestedDecoder)) )
                , ( "function"
                  , JD.map2 FunctionTypeAnnotation
                        (JD.field "left" nestedDecoder)
                        (JD.field "right" nestedDecoder)
                  )
                , ( "record"
                  , JD.map Record (JD.field "value" (JD.list <| Node.decoder recordFieldDecoder))
                  )
                , ( "extension"
                  , JD.map3 ExtensionRecord
                        (JD.field "generic" (Node.decoder JD.string))
                        (JD.field "firstField" (Node.decoder recordFieldDecoder))
                        (JD.field "restOfFields" (JD.list <| Node.decoder recordFieldDecoder))
                  )
                ]
        )


nestedDecoder : Decoder (Node Range TypeAnnotation)
nestedDecoder =
    JD.lazy (\() -> Node.decoder decoder)


recordFieldDecoder : Decoder RecordField
recordFieldDecoder =
    JD.lazy
        (\() ->
            JD.map2 Tuple.pair
                (JD.field "name" <| Node.decoder JD.string)
                (JD.field "typeAnnotation" nestedDecoder)
        )
