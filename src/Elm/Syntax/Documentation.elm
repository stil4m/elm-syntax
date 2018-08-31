module Elm.Syntax.Documentation exposing
    ( Documentation
    , range
    , encode, decoder
    )

{-| Documenation Syntax


# Types

@docs Documentation


# Functions

@docs range


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Documentation comment
-}
type alias Documentation =
    Node String



-- Functions


{-| Get the source range of the documentation
-}
range : Documentation -> Range
range (Node r _) =
    r



-- Serialization


{-| Encode a `Documentation` syntax element to JSON.
-}
encode : Documentation -> Value
encode =
    Node.encode JE.string


{-| JSON decoder for a `Documentation` syntax element.
-}
decoder : Decoder Documentation
decoder =
    Node.decoder JD.string
