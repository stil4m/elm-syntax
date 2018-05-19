module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing (Parser, between, choice, sepBy1, string, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, moduleToken, portToken, typeName)
import Elm.Syntax.Module exposing (DefaultModuleData, Module(..))


moduleDefinition : Parser State Module
moduleDefinition =
    choice
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, String )
effectWhereClause =
    succeed (\a b -> ( a, b ))
        |> Combine.andMap functionName
        |> Combine.andMap (Layout.maybeAroundBothSides (string "=") |> Combine.continueWith typeName)


whereBlock : Parser State { command : Maybe String, subscription : Maybe String }
whereBlock =
    between
        (string "{")
        (string "}")
        (sepBy1 (string ",")
            (Layout.maybeAroundBothSides effectWhereClause)
        )
        |> Combine.map
            (\pairs ->
                { command = pairs |> List.filter (Tuple.first >> (==) "command") |> List.head |> Maybe.map Tuple.second
                , subscription = pairs |> List.filter (Tuple.first >> (==) "subscription") |> List.head |> Maybe.map Tuple.second
                }
            )


effectWhereClauses : Parser State { command : Maybe String, subscription : Maybe String }
effectWhereClauses =
    string "where"
        |> Combine.continueWith Layout.layout
        |> Combine.continueWith whereBlock


effectModuleDefinition : Parser State Module
effectModuleDefinition =
    let
        createEffectModule name whereClauses exp =
            EffectModule
                { moduleName = name
                , exposingList = exp
                , command = whereClauses.command
                , subscription = whereClauses.subscription
                }
    in
    succeed createEffectModule
        |> Combine.andMap
            (string "effect"
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleName
            )
        |> Combine.andMap (Layout.layout |> Combine.continueWith effectWhereClauses)
        |> Combine.andMap (exposeDefinition exposable)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    Combine.map NormalModule
        (succeed DefaultModuleData
            |> Combine.andMap (moduleToken |> Combine.continueWith Layout.layout |> Combine.continueWith moduleName)
            |> Combine.andMap (exposeDefinition exposable)
        )


portModuleDefinition : Parser State Module
portModuleDefinition =
    Combine.map PortModule
        (succeed DefaultModuleData
            |> Combine.andMap
                (portToken
                    |> Combine.continueWith Layout.layout
                    |> Combine.continueWith moduleToken
                    |> Combine.continueWith Layout.layout
                    |> Combine.continueWith moduleName
                )
            |> Combine.andMap (exposeDefinition exposable)
        )
