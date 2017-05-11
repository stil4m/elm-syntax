module Elm.Processing.ModuleIndex exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Interface as Interface exposing (Interface)
import Elm.Dependency exposing (Dependency)
import Elm.RawFile as RawFile
import Elm.Internal.RawFile exposing (RawFile(Raw))
import Elm.DefaultImports as DefaultImports
import Elm.Syntax.Module as AST exposing (..)
import Elm.Syntax.Infix exposing (Infix, InfixDirection)
import Elm.Syntax.Exposing as AST exposing (..)


type alias OperatorTable =
    Dict String Infix


type ModuleIndex
    = ModuleIndex ModuleIndexInner


type alias ModuleIndexInner =
    Dict ModuleName Interface


init : ModuleIndex
init =
    ModuleIndex Dict.empty


addFile : RawFile -> ModuleIndex -> ModuleIndex
addFile file ((ModuleIndex x) as m) =
    case entryFromRawFile file of
        Just ( k, v ) ->
            ModuleIndex (Dict.insert k v x)

        Nothing ->
            m


addDependency : Dependency -> ModuleIndex -> ModuleIndex
addDependency dep ((ModuleIndex x) as m) =
    ModuleIndex (Dict.foldl (\k v d -> Dict.insert k v d) x dep.interfaces)


entryFromRawFile : RawFile -> Maybe ( ModuleName, Interface )
entryFromRawFile ((Raw file) as rawFile) =
    case (RawFile.moduleName rawFile) of
        Just modName ->
            Just ( modName, Interface.build rawFile )

        Nothing ->
            Nothing


build : RawFile -> ModuleIndex -> OperatorTable
build rawFile (ModuleIndex moduleIndex) =
    List.concatMap (flip buildSingle moduleIndex) (DefaultImports.defaults ++ RawFile.imports rawFile)
        |> Dict.fromList


buildSingle : Import -> ModuleIndexInner -> List ( String, Infix )
buildSingle imp moduleIndex =
    case imp.exposingList of
        AST.None ->
            []

        AST.All _ ->
            moduleIndex
                |> Dict.get imp.moduleName
                |> Maybe.withDefault []
                |> Interface.operators
                |> List.map (\x -> ( x.operator, x ))

        AST.Explicit l ->
            let
                selectedOperators =
                    List.filterMap getExposedOperators l
            in
                moduleIndex
                    |> Dict.get imp.moduleName
                    |> Maybe.withDefault []
                    |> Interface.operators
                    |> List.map (\x -> ( x.operator, x ))
                    |> List.filter (Tuple.first >> flip List.member selectedOperators)


getExposedOperators : AST.TopLevelExpose -> Maybe String
getExposedOperators x =
    case x of
        AST.InfixExpose s _ ->
            Just s

        _ ->
            Nothing
