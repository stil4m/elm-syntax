module Elm.Syntax.Infix exposing (Infix, InfixDirection(..), decode, decodeDirection, encode, encodeDirection)

{-| Infix Syntax

#Types

@docs Infix, InfixDirection


# Json

@docs encode, encodeDirection, decode, decodeDirection

-}

import Elm.Syntax.Range as Range
import Elm.Syntax.Ranged exposing (Ranged)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Ranged InfixDirection
    , precedence : Ranged Int
    , operator : Ranged String
    , function : Ranged String
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
        [ ( "direction"
          , JE.object
                [ ( "range", Range.encode <| Tuple.first inf.direction )
                , ( "value", encodeDirection <| Tuple.second inf.direction )
                ]
          )
        , ( "precedence"
          , JE.object
                [ ( "range", Range.encode <| Tuple.first inf.precedence )
                , ( "value", JE.int <| Tuple.second inf.precedence )
                ]
          )
        , ( "operator"
          , JE.object
                [ ( "range", Range.encode <| Tuple.first inf.operator )
                , ( "value", JE.string <| Tuple.second inf.operator )
                ]
          )
        , ( "function"
          , JE.object
                [ ( "range", Range.encode <| Tuple.first inf.function )
                , ( "value", JE.string <| Tuple.second inf.function )
                ]
          )
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


decodeRanged : Decoder a -> Decoder (Ranged a)
decodeRanged sub =
    JD.map2 Tuple.pair
        (JD.field "range" Range.decode)
        (JD.field "value" sub)


{-| Decode an infix
-}
decode : Decoder Infix
decode =
    JD.map4 Infix
        (JD.field "direction" (decodeRanged decodeDirection))
        (JD.field "precedence" (decodeRanged JD.int))
        (JD.field "operator" (decodeRanged JD.string))
        (JD.field "function" (decodeRanged JD.string))


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
