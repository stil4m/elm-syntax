module Elm.Syntax.TypeAlias exposing
    ( TypeAlias
    , encode, decoder
    )

{-|


# Exposing Syntax

This syntax represents type aliases.
For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }


# Types

@docs TypeAlias


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias that defines the syntax for a type alias.
A bit meta, but you get the idea. All information that you can define in a type alias is embedded.
-}
type alias TypeAlias r =
    { documentation : Maybe (Node r Documentation)
    , name : Node r String
    , generics : List (Node r String)
    , typeAnnotation : Node r (TypeAnnotation r)
    }



-- Serialization


{-| Encode a `TypeAlias` syntax element to JSON.
-}
encode : TypeAlias r -> Value
encode { documentation, name, generics, typeAnnotation } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "name", Node.encode JE.string name )
        , ( "generics", JE.list (Node.encode JE.string) generics )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]


{-| JSON decoder for a `Declaration` syntax element.
-}
decoder : Decoder (TypeAlias r)
decoder =
    JD.map4 TypeAlias
        (JD.field "documentation" (JD.nullable <| Node.decoder Documentation.decoder))
        (JD.field "name" <| Node.decoder JD.string)
        (JD.field "generics" (JD.list <| Node.decoder JD.string))
        (JD.field "typeAnnotation" (Node.decoder TypeAnnotation.decoder))
