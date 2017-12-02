module Elm.Syntax.File exposing (File)

{-| File Syntax


# Types

@docs File

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Import, Module)
import Elm.Syntax.Ranged exposing (Ranged)


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Module
    , imports : List Import
    , declarations : List (Ranged Declaration)
    , comments : List (Ranged String)
    }
