module Elm.OperatorTable exposing (OperatorTable, ModuleIndex, build, buildModuleIndex)

import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Range as Range
import Elm.Syntax.Infix exposing (Infix, InfixDirection)
import Elm.Syntax.Module as AST exposing (..)
import Elm.Dependency exposing (Dependency)
import Elm.Interface as Interface exposing (Interface)
import Elm.Interface as Interface
import Dict exposing (Dict)
import Elm.Syntax.Exposing as AST exposing (..)
import Elm.RawFile as RawFile
import Elm.Internal.RawFile as RawFile exposing (RawFile(Raw))
import Elm.Interface as Interface
import Elm.Syntax.Base exposing (ModuleName)


type alias OperatorTable =
    Dict String Infix


type ModuleIndex
    = ModuleIndex ModuleIndexInner


type alias ModuleIndexInner =
    Dict ModuleName Interface


buildModuleIndex : List RawFile -> List Dependency -> ModuleIndex
buildModuleIndex sourceFiles dependencies =
    List.filterMap entryFromRawFile sourceFiles
        ++ (dependencies |> List.concatMap (.interfaces >> Dict.toList))
        |> Dict.fromList
        |> ModuleIndex


entryFromRawFile : RawFile -> Maybe ( ModuleName, Interface )
entryFromRawFile ((Raw file) as rawFile) =
    case (RawFile.moduleName rawFile) of
        Just modName ->
            Just ( modName, Interface.build rawFile )

        Nothing ->
            Nothing


build : RawFile -> ModuleIndex -> OperatorTable
build rawFile (ModuleIndex moduleIndex) =
    List.concatMap (flip buildSingle moduleIndex) (defaultImports ++ RawFile.imports rawFile)
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


defaultImports : List AST.Import
defaultImports =
    [ { moduleName = [ "Basics" ]
      , exposingList = AST.All Range.emptyRange
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "List" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "List" AST.None Range.emptyRange)
                , (InfixExpose "::" Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Maybe" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose
                    (ExposedType "Maybe"
                        (AST.Explicit
                            [ ( "Just", Range.emptyRange )
                            , ( "Nothing", Range.emptyRange )
                            ]
                        )
                        Range.emptyRange
                    )
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Result" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose
                    (ExposedType "Result"
                        (AST.Explicit
                            [ ( "Ok", Range.emptyRange )
                            , ( "Err", Range.emptyRange )
                            ]
                        )
                        Range.emptyRange
                    )
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "String" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Tuple" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Debug" ], exposingList = AST.None, moduleAlias = Nothing, range = Range.emptyRange }
    , { moduleName = [ "Platform" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Program" AST.None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Cmd" AST.None Range.emptyRange)
                , (InfixExpose "!" Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    , { moduleName = [ "Platform", "Sub" ]
      , exposingList =
            AST.Explicit
                [ TypeExpose (ExposedType "Sub" AST.None Range.emptyRange)
                ]
      , moduleAlias = Nothing
      , range = Range.emptyRange
      }
    ]
