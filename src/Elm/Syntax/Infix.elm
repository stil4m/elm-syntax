module Elm.Syntax.Infix exposing
    ( Infix, InfixDirection(..)
    , encode, encodeDirection, decode, decodeDirection
    )

{-| Infix Syntax

#Types

@docs Infix, InfixDirection


# Json

@docs encode, encodeDirection, decode, decodeDirection

-}

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra exposing ((|:))
import Json.Encode as JE exposing (Value)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right


{-| Encode an infix
-}
encode : Infix -> Value
encode inf =
    JE.object
        [ ( "direction", encodeDirection inf.direction )
        , ( "precedence", JE.int inf.precedence )
        , ( "operator", JE.string inf.operator )
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


{-| Decode an infix
-}
decode : Decoder Infix
decode =
    JD.succeed Infix
        |: JD.field "direction" decodeDirection
        |: JD.field "precedence" JD.int
        |: JD.field "operator" JD.string


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

                    _ ->
                        JD.fail "Invlalid direction"
            )
