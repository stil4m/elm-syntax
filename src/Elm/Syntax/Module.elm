module Elm.Syntax.Module exposing
    ( Module(..), DefaultModuleData, EffectModuleData
    , exposingList, moduleName, isPortModule, isEffectModule
    , Import
    )

{-| Module Syntax


# Module

@docs Module, DefaultModuleData, EffectModuleData

@docs exposingList, moduleName, isPortModule, isEffectModule


# Import

@docs Import

-}

import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)


{-| Union type for different kind of modules
-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    , command : Maybe String
    , subscription : Maybe String
    }


{-| Import definition
-}
type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Maybe Exposing
    , range : Range
    }


{-| Get the name for a module. For older modules this may not be present.
-}
moduleName : Module -> Maybe ModuleName
moduleName m =
    case m of
        NormalModule x ->
            Just x.moduleName

        PortModule x ->
            Just x.moduleName

        EffectModule x ->
            Just x.moduleName


{-| Get the exposing list for a module.
-}
exposingList : Module -> Exposing
exposingList m =
    case m of
        NormalModule x ->
            x.exposingList

        PortModule x ->
            x.exposingList

        EffectModule x ->
            x.exposingList


{-| Check whether a module is defined as a port-module
-}
isPortModule : Module -> Bool
isPortModule m =
    case m of
        PortModule _ ->
            True

        _ ->
            False


{-| Check whether a module is defined as an effect-module
-}
isEffectModule : Module -> Bool
isEffectModule m =
    case m of
        EffectModule _ ->
            True

        _ ->
            False
