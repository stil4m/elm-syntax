module Elm.RawFile exposing (RawFile, moduleName, imports)

{-|


# Elm.RawFile

@docs RawFile

@docs moduleName, imports

-}

import Elm.Internal.RawFile as Internal
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Module exposing (Import)


{-| A Raw file
-}
type alias RawFile =
    Internal.RawFile


{-| TODO
-}
moduleName : RawFile -> Maybe ModuleName
moduleName (Internal.Raw file) =
    Module.moduleName file.moduleDefinition


{-| TODO
-}
imports : RawFile -> List Import
imports (Internal.Raw file) =
    file.imports
