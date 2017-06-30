module Elm.RawFile exposing (RawFile, moduleName, imports)

{-|


# Elm.RawFile

@docs RawFile

@docs moduleName, imports

-}

import Elm.Internal.RawFile as Internal
import Elm.Syntax.Module as Module exposing (Import)
import Elm.Syntax.Base exposing (ModuleName)


{-| A Raw file
-}
type alias RawFile =
    Internal.RawFile


{-| Retrieve the module name for a raw file
-}
moduleName : RawFile -> Maybe ModuleName
moduleName (Internal.Raw file) =
    Module.moduleName file.moduleDefinition


{-| Retrieve the imports for a raw file
-}
imports : RawFile -> List Import
imports (Internal.Raw file) =
    file.imports
