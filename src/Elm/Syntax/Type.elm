module Elm.Syntax.Type exposing
    ( Type, ValueConstructor
    , encode, decoder
    )

{-| Type Syntax


# Types

@docs Type, ValueConstructor


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Syntax for a type
-}
type alias Type =
    { name : Node String
    , generics : List (Node String)
    , constructors : List (Node ValueConstructor)
    }


{-| Syntax for a type value constructor
-}
type alias ValueConstructor =
    { name : Node String
    , arguments : List (Node TypeAnnotation)
    }



-- Serialization


encode : Type -> Value
encode { name, generics, constructors } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "generics", JE.list (Node.encode JE.string) generics )
        , ( "constructors", JE.list (Node.encode encodeValueConstructor) constructors )
        ]


encodeValueConstructor : ValueConstructor -> Value
encodeValueConstructor { name, arguments } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "arguments", JE.list (Node.encode TypeAnnotation.encode) arguments )
        ]


decoder : Decoder Type
decoder =
    JD.map3 Type
        (JD.field "name" <| Node.decoder JD.string)
        (JD.field "generics" (JD.list (Node.decoder JD.string)))
        (JD.field "constructors" (JD.list (Node.decoder valueConstructorDecoder)))


valueConstructorDecoder : Decoder ValueConstructor
valueConstructorDecoder =
    JD.map2 ValueConstructor
        (JD.field "name" (Node.decoder JD.string))
        (JD.field "arguments" (JD.list (Node.decoder TypeAnnotation.decoder)))
