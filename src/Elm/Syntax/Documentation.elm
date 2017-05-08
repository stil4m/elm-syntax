module Elm.Syntax.Documentation exposing (Documentation)

{-| Documenation Syntax


# Types

@docs Documentation

-}

import Elm.Syntax.Range exposing (Range)


{-| Documentation comment
-}
type alias Documentation =
    { text : String
    , range : Range
    }
