module Elm.Syntax.ModuleName exposing
    ( ModuleName
    , encode
    , decoder
    )

{-| Base Syntax


# Types

@docs ModuleName


# Serialization

@docs encode, decode

-}

import Elm.Syntax.Range as Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Base representation for a module name
-}
type alias ModuleName =
    List String



-- Serialization


encode : ModuleName -> Value
encode =
    JE.list JE.string


decoder : Decoder ModuleName
decoder =
    JD.list JD.string
