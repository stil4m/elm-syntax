module Elm.Syntax.Infix exposing (Infix, InfixDirection(Left, Right), encode, encodeDirection)

{-| Infix Syntax

#Types

@docs Infix, InfixDirection


# Json

@docs encode, encodeDirection

-}

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
