module Elm.Dependency exposing (Dependency, Version)

{-|


# Elm.Dependency

@docs Dependency, Version

-}

import Dict exposing (Dict)
import Elm.Interface exposing (Interface)
import Elm.Syntax.Base exposing (ModuleName)


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
