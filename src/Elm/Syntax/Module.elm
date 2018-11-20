module Elm.Syntax.Module exposing
    ( Module(..), DefaultModuleData, EffectModuleData
    , exposingList, moduleName, isPortModule, isEffectModule
    , encode, decoder
    )

{-|


# Module Syntax

This syntax represents module definitions in Elm.
For example:

    module Html.Attributes (style)


## Module

@docs Module, DefaultModuleData, EffectModuleData

@docs exposingList, moduleName, isPortModule, isEffectModule


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..))
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Union type for different kind of modules
-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    , command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }


{-| Get the name for a module. For older modules this may not be present.
-}
moduleName : Module -> ModuleName
moduleName m =
    case m of
        NormalModule x ->
            Node.value x.moduleName

        PortModule x ->
            Node.value x.moduleName

        EffectModule x ->
            Node.value x.moduleName


{-| Get the exposing list for a module.
-}
exposingList : Module -> Exposing
exposingList m =
    case m of
        NormalModule x ->
            Node.value x.exposingList

        PortModule x ->
            Node.value x.exposingList

        EffectModule x ->
            Node.value x.exposingList


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



-- Serialization


{-| Encode a `Module` syntax element to JSON.
-}
encode : Module -> Value
encode m =
    case m of
        NormalModule d ->
            encodeTyped "normal" (encodeDefaultModuleData d)

        PortModule d ->
            encodeTyped "port" (encodeDefaultModuleData d)

        EffectModule d ->
            encodeTyped "effect" (encodeEffectModuleData d)


encodeEffectModuleData : EffectModuleData -> Value
encodeEffectModuleData moduleData =
    JE.object
        [ ( "moduleName", Node.encode ModuleName.encode moduleData.moduleName )
        , ( "exposingList", Node.encode Exposing.encode moduleData.exposingList )
        , ( "command", moduleData.command |> Maybe.map (Node.encode JE.string) |> Maybe.withDefault JE.null )
        , ( "subscription", moduleData.subscription |> Maybe.map (Node.encode JE.string) |> Maybe.withDefault JE.null )
        ]


encodeDefaultModuleData : DefaultModuleData -> Value
encodeDefaultModuleData moduleData =
    JE.object
        [ ( "moduleName", Node.encode ModuleName.encode moduleData.moduleName )
        , ( "exposingList", Node.encode Exposing.encode moduleData.exposingList )
        ]


{-| JSON decoder for a `Module` syntax element.
-}
decoder : Decoder Module
decoder =
    decodeTyped
        [ ( "normal", decodeDefaultModuleData |> JD.map NormalModule )
        , ( "port", decodeDefaultModuleData |> JD.map PortModule )
        , ( "effect", decodeEffectModuleData |> JD.map EffectModule )
        ]


decodeDefaultModuleData : Decoder DefaultModuleData
decodeDefaultModuleData =
    JD.map2 DefaultModuleData
        (JD.field "moduleName" <| Node.decoder ModuleName.decoder)
        (JD.field "exposingList" <| Node.decoder Exposing.decoder)


decodeEffectModuleData : Decoder EffectModuleData
decodeEffectModuleData =
    JD.map4 EffectModuleData
        (JD.field "moduleName" <| Node.decoder ModuleName.decoder)
        (JD.field "exposingList" <| Node.decoder Exposing.decoder)
        (JD.field "command" (JD.nullable <| Node.decoder JD.string))
        (JD.field "subscription" (JD.nullable <| Node.decoder JD.string))
