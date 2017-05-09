module Elm.Syntax.Module
    exposing
        ( Module(..)
        , DefaultModuleData
        , EffectModuleData
        , Import
        )

{-| Module Syntax


# Module

@docs Module, DefaultModuleData, EffectModuleData


# Import

@docs Import

-}

import Elm.Syntax.Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Base exposing (ModuleName)


{-| Union type for different kind of modules
-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData
    | NoModule


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing TopLevelExpose
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing TopLevelExpose
    , command : Maybe String
    , subscription : Maybe String
    }


{-| Import definition
-}
type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Exposing TopLevelExpose
    , range : Range
    }
