module Elm.Syntax.File exposing (File)

{-| File Syntax


# Types

@docs File

-}

import Elm.Syntax.Module exposing (Module, Import)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Range exposing (Range)


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Module
    , imports : List Import
    , declarations : List Declaration
    , comments : List ( String, Range )
    }
