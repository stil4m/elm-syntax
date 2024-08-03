module Elm.Syntax.Module exposing (Module, ModuleKind(..), EffectModuleData)

{-| This syntax represents module definitions in Elm.
For example:

    module Html.Attributes exposing (style)


## Module

@docs Module, ModuleKind, EffectModuleData

-}

import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


{-| Type alias representing a Module.
-}
type alias Module =
    { moduleName : Node ModuleName
    , kind : ModuleKind
    , exposingList : Node Exposing
    }


{-| Union type for different kind of modules
-}
type ModuleKind
    = NormalModule
    | PortModule
    | EffectModule EffectModuleData


{-| Data for an effect module
-}
type alias EffectModuleData =
    { command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }
