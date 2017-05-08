module Elm.Syntax.Module
    exposing
        ( Module(..)
        , DefaultModuleData
        , EffectModuleData
        , Exposure(..)
        , Expose(..)
        , ExposedType
        , ValueConstructorExpose
        , Import
        )

{-| Module Syntax


# Module

@docs Module, DefaultModuleData, EffectModuleData


# Import

@docs Import


# Exposing

@docs Exposure, Expose, ExposedType, ValueConstructorExpose

-}

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
    , exposingList : Exposure Expose
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposure Expose
    , command : Maybe String
    , subscription : Maybe String
    }


{-| Diffent kind of exposing declarations
-}
type Exposure a
    = None
    | All Range
    | Explicit (List a)


{-| An exposed entity
-}
type Expose
    = InfixExpose String Range
    | FunctionExpose String Range
    | TypeOrAliasExpose String Range
    | TypeExpose ExposedType


{-| Exposed Type
-}
type alias ExposedType =
    { name : String
    , constructors : Exposure ValueConstructorExpose
    , range : Range
    }


{-| Exposed Value Constructor
-}
type alias ValueConstructorExpose =
    ( String, Range )


{-| Import definition
-}
type alias Import =
    { moduleName : ModuleName
    , moduleAlias : Maybe ModuleName
    , exposingList : Exposure Expose
    , range : Range
    }
