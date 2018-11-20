module Elm.Syntax.Documentation exposing
    ( Documentation
    , encode, decoder
    )

{-|


# Documenation Syntax

This syntax represents documentation comments in Elm.


## Types

@docs Documentation


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node exposing (Node(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type representing the documentation syntax
-}
type alias Documentation =
    String



-- Serialization


{-| Encode a `Documentation` syntax element to JSON.
-}
encode : Documentation -> Value
encode =
    JE.string


{-| JSON decoder for a `Documentation` syntax element.
-}
decoder : Decoder Documentation
decoder =
    JD.string
