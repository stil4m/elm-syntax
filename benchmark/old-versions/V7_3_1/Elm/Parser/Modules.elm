module V7_3_1.Elm.Parser.Modules exposing (moduleDefinition)

import V7_3_1.Combine as Combine exposing (Parser, between, choice, sepBy1, string, succeed)
import V7_3_1.Elm.Parser.Base exposing (moduleName)
import V7_3_1.Elm.Parser.Expose exposing (exposeDefinition)
import V7_3_1.Elm.Parser.Layout as Layout
import V7_3_1.Elm.Parser.Node as Node
import V7_3_1.Elm.Parser.State exposing (State)
import V7_3_1.Elm.Parser.Tokens exposing (functionName, moduleToken, portToken, typeName)
import V7_3_1.Elm.Syntax.Module exposing (DefaultModuleData, Module(..))
import V7_3_1.Elm.Syntax.Node exposing (Node)


moduleDefinition : Parser State Module
moduleDefinition =
    choice
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, Node String )
effectWhereClause =
    succeed Tuple.pair
        |> Combine.andMap functionName
        |> Combine.andMap (Layout.maybeAroundBothSides (string "=") |> Combine.continueWith (Node.parser typeName))


whereBlock : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
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


effectWhereClauses : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
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
        |> Combine.ignore (string "effect")
        |> Combine.ignore Layout.layout
        |> Combine.ignore moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser moduleName)
        |> Combine.ignore Layout.layout
        |> Combine.andMap effectWhereClauses
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser exposeDefinition)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    Combine.map NormalModule
        (succeed DefaultModuleData
            |> Combine.ignore moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser exposeDefinition)
        )


portModuleDefinition : Parser State Module
portModuleDefinition =
    Combine.map PortModule
        (succeed DefaultModuleData
            |> Combine.ignore portToken
            |> Combine.ignore Layout.layout
            |> Combine.ignore moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.andMap (Node.parser exposeDefinition)
        )
