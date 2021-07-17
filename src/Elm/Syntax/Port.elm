module Elm.Syntax.Port exposing (Port, decoder, encode)

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias Port =
    { documentation : Maybe (Node Range Documentation)
    , signature : Node Range Signature
    }


encode : Port -> Value
encode { documentation, signature } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "signature", Node.encode Signature.encode signature )
        ]


decoder : Decoder Port
decoder =
    JD.map2 Port
        (JD.field "documentation" (JD.nullable <| Node.decoder Documentation.decoder))
        (JD.field "signature" (Node.decoder Signature.decoder))
