module Elm.Syntax.Signature exposing (Signature)

{-| This syntax represents type signatures in Elm.

For example :

    add : Int -> Int -> Int


## Types

@docs Signature

-}

import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


{-| Type alias representing a signature in Elm.
-}
type alias Signature =
    { name : Node String
    , typeAnnotation : Node TypeAnnotation
    }
