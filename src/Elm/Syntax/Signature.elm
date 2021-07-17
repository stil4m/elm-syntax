module Elm.Syntax.Signature exposing
    ( Signature
    , encode, decoder
    )

{-|


# Signature Syntax

For example :

    add : Int -> Int -> Int


## Types

@docs Signature


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias representing a signature in Elm.
-}
type alias Signature r =
    { name : Node r String
    , typeAnnotation : Node r (TypeAnnotation r)
    }


{-| Encode a `Signature` syntax element to JSON.
-}
encode : Signature r -> Value
encode { name, typeAnnotation } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]


{-| JSON decoder for a `Signature` syntax element.
-}
decoder : Decoder (Signature r)
decoder =
    JD.map2 Signature
        (JD.field "name" (Node.decoder JD.string))
        (JD.field "typeAnnotation" (Node.decoder TypeAnnotation.decoder))
