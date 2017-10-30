module Elm.Syntax.Module
    exposing
        ( DefaultModuleData
        , EffectModuleData
        , Import
        , Module(EffectModule, NormalModule, PortModule)
        , exposingList
        , moduleName
        )

{-| Module Syntax


# Module

@docs Module, DefaultModuleData, EffectModuleData

@docs exposingList, moduleName


# Import

@docs Import

-}

import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.Range exposing (Range)


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
    , exposingList : Maybe (Exposing TopLevelExpose)
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
exposingList : Module -> Exposing TopLevelExpose
exposingList m =
    case m of
        NormalModule x ->
            x.exposingList

        PortModule x ->
            x.exposingList

        EffectModule x ->
            x.exposingList
