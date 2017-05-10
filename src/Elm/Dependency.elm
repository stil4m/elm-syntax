module Elm.Dependency exposing (Dependency, Version)

{-|


# Elm.Dependency

@docs Dependency, Version

-}

import Dict exposing (Dict)
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Interface exposing (Interface)


{-| Record that represents a dependency
-}
type alias Dependency =
    { name : String
    , version : Version
    , interfaces : Dict ModuleName Interface
    }


{-| Alias for a version
-}
type alias Version =
    String
