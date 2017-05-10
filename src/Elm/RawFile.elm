module Elm.RawFile exposing (RawFile, moduleName)

{-|


# Elm.RawFile

@docs RawFile

@docs moduleName

-}

import Elm.Internal.RawFile as Internal
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Base exposing (ModuleName)


{-| A Raw file
-}
type alias RawFile =
    Internal.RawFile


{-| TODO
-}
moduleName : RawFile -> Maybe ModuleName
moduleName (Internal.Raw file) =
    Module.moduleName file.moduleDefinition
