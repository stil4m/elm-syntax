module Elm.Syntax.Port exposing
    ( Port
    , encode, decoder
    )

{-| This syntax represents ports.
For example:

    {-| This is a port.
    -}
    port send : String -> Cmd msg

    {-| This is another port.
    -}
    port scroll : (Move -> msg) -> Sub msg


## Types

@docs Port


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Information associated with a Port.
A bit meta, but you get the idea. All information that you can define in a type alias is embedded.
-}
type alias Port =
    { documentation : Maybe (Node Documentation)
    , signature : Node Signature
    }


{-| Encode a `Port` syntax element to JSON.
-}
encode : Port -> Value
encode { documentation, signature } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "signature", Node.encode Signature.encode signature )
        ]


{-| JSON decoder for a `Port` syntax element.
-}
decoder : Decoder Port
decoder =
    JD.map2 Port
        (JD.field "documentation" (JD.nullable <| Node.decoder Documentation.decoder))
        (JD.field "signature" (Node.decoder Signature.decoder))
