module Elm.Syntax.Type exposing
    ( Type, ValueConstructor
    , encode, decoder
    )

{-|


# Type Syntax

This syntax represents custom types.
For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red


## Types

@docs Type, ValueConstructor


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias that defines the syntax for a custom type.
All information that you can define in a type alias is embedded.
-}
type alias Type =
    { documentation : Maybe (Node Range Documentation)
    , name : Node Range String
    , generics : List (Node Range String)
    , firstConstructor : Node Range ValueConstructor
    , restOfConstructors : List (Node Range ValueConstructor)
    }


{-| Syntax for a custom type value constructor.
-}
type alias ValueConstructor =
    { name : Node Range String
    , arguments : List (Node Range TypeAnnotation)
    }



-- Serialization


{-| Encode a `Type` syntax element to JSON.
-}
encode : Type -> Value
encode { documentation, name, generics, firstConstructor, restOfConstructors } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "name", Node.encode JE.string name )
        , ( "generics", JE.list (Node.encode JE.string) generics )
        , ( "firstConstructor", Node.encode encodeValueConstructor firstConstructor )
        , ( "restOfConstructors", JE.list (Node.encode encodeValueConstructor) restOfConstructors )
        ]


encodeValueConstructor : ValueConstructor -> Value
encodeValueConstructor { name, arguments } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "arguments", JE.list (Node.encode TypeAnnotation.encode) arguments )
        ]


{-| JSON decoder for a `Type` syntax element.
-}
decoder : Decoder Type
decoder =
    JD.map5 Type
        (JD.field "documentation" <| JD.nullable <| Node.decoder JD.string)
        (JD.field "name" <| Node.decoder JD.string)
        (JD.field "generics" (JD.list (Node.decoder JD.string)))
        (JD.field "firstConstructor" (Node.decoder valueConstructorDecoder))
        (JD.field "restOfConstructors" (JD.list (Node.decoder valueConstructorDecoder)))


valueConstructorDecoder : Decoder ValueConstructor
valueConstructorDecoder =
    JD.map2 ValueConstructor
        (JD.field "name" (Node.decoder JD.string))
        (JD.field "arguments" (JD.list (Node.decoder TypeAnnotation.decoder)))
