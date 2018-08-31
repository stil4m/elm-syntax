module Elm.Syntax.TypeAlias exposing
    ( TypeAlias
    , encode, decoder
    )

{-| Type Alias Syntax


# Types

@docs TypeAlias


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Definition for a type alias
-}
type alias TypeAlias =
    { documentation : Maybe Documentation
    , name : Node String
    , generics : List (Node String)
    , typeAnnotation : Node TypeAnnotation
    }



-- Serialization


encode : TypeAlias -> Value
encode { documentation, name, generics, typeAnnotation } =
    JE.object
        [ ( "documentation", Maybe.map Documentation.encode documentation |> Maybe.withDefault JE.null )
        , ( "name", Node.encode JE.string name )
        , ( "generics", JE.list (Node.encode JE.string) generics )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]


decoder : Decoder TypeAlias
decoder =
    JD.map4 TypeAlias
        (JD.field "documentation" (JD.nullable Documentation.decoder))
        (JD.field "name" <| Node.decoder JD.string)
        (JD.field "generics" (JD.list <| Node.decoder JD.string))
        (JD.field "typeAnnotation" (Node.decoder TypeAnnotation.decoder))
