module Elm.Syntax.Type exposing (Type, ValueConstructor)

{-| Type Syntax


# Types

@docs Type, ValueConstructor

-}

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


{-| Syntax for a type
-}
type alias Type =
    { name : String
    , generics : List String
    , constructors : List ValueConstructor
    }


{-| Syntax for a type value constructor
-}
type alias ValueConstructor =
    { name : String
    , arguments : List (Ranged TypeAnnotation)
    , range : Range
    }
