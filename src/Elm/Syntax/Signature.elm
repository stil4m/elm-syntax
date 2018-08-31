module Elm.Syntax.Signature exposing
    ( Signature
    , encode, decoder
    )

{-| Signature Syntax


# Types

@docs Signature


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias Signature =
    { name : Node String
    , typeAnnotation : Node TypeAnnotation
    }


encode : Signature -> Value
encode { name, typeAnnotation } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]


decoder : Decoder Signature
decoder =
    JD.map2 Signature
        (JD.field "name" (Node.decoder JD.string))
        (JD.field "typeAnnotation" (Node.decoder TypeAnnotation.decoder))
