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
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias that deines the syntax for a type alias.
A bit meta, but you get the idea. All information that you can define in on type alias is embedded.
-}
type alias TypeAlias =
    { documentation : Maybe (Node Documentation)
    , name : Node String
    , generics : List (Node String)
    , typeAnnotation : Node TypeAnnotation
    }



-- Serialization


{-| Encode a `TypeAlias` syntax element to JSON.
-}
encode : TypeAlias -> Value
encode { documentation, name, generics, typeAnnotation } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "name", Node.encode JE.string name )
        , ( "generics", JE.list (Node.encode JE.string) generics )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]


{-| JSON decoder for a `Declaration` syntax element.
-}
decoder : Decoder TypeAlias
decoder =
    JD.map4 TypeAlias
        (JD.field "documentation" (JD.nullable <| Node.decoder Documentation.decoder))
        (JD.field "name" <| Node.decoder JD.string)
        (JD.field "generics" (JD.list <| Node.decoder JD.string))
        (JD.field "typeAnnotation" (Node.decoder TypeAnnotation.decoder))
