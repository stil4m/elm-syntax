module Elm.Syntax.Infix exposing
    ( Infix, InfixDirection(..)
    , encode, encodeDirection, decoder, decodeDirection
    )

{-|


## Types

@docs Infix, InfixDirection


## Serialization

@docs encode, encodeDirection, decoder, decodeDirection

-}

import Elm.Syntax.Node as Node exposing (Node)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non


{-| Encode an infix
-}
encode : Infix -> Value
encode inf =
    JE.object
        [ ( "direction", Node.encode encodeDirection inf.direction )
        , ( "precedence", Node.encode JE.int inf.precedence )
        , ( "operator", Node.encode JE.string inf.operator )
        , ( "function", Node.encode JE.string inf.function )
        ]


{-| Encode the infix direction
-}
encodeDirection : InfixDirection -> Value
encodeDirection d =
    case d of
        Left ->
            JE.string "left"

        Right ->
            JE.string "right"

        Non ->
            JE.string "non"


{-| Decode an infix
-}
decoder : Decoder Infix
decoder =
    JD.map4 Infix
        (JD.field "direction" (Node.decoder decodeDirection))
        (JD.field "precedence" (Node.decoder JD.int))
        (JD.field "operator" (Node.decoder JD.string))
        (JD.field "function" (Node.decoder JD.string))


{-| Decode a infix direction
-}
decodeDirection : Decoder InfixDirection
decodeDirection =
    JD.string
        |> JD.andThen
            (\v ->
                case v of
                    "left" ->
                        JD.succeed Left

                    "right" ->
                        JD.succeed Right

                    "non" ->
                        JD.succeed Non

                    _ ->
                        JD.fail "Invlalid direction"
            )
