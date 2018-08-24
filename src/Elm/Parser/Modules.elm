module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing ((*>), (<$>), (<*>), Parser, between, choice, sepBy1, string, succeed)
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
        <*> functionName
        <*> (Layout.maybeAroundBothSides (string "=") *> typeName)


whereBlock : Parser State { command : Maybe String, subscription : Maybe String }
whereBlock =
    (\pairs ->
        { command = pairs |> List.filter (Tuple.first >> (==) "command") |> List.head |> Maybe.map Tuple.second
        , subscription = pairs |> List.filter (Tuple.first >> (==) "subscription") |> List.head |> Maybe.map Tuple.second
        }
    )
        <$> between
                (string "{")
                (string "}")
                (sepBy1 (string ",")
                    (Layout.maybeAroundBothSides effectWhereClause)
                )


effectWhereClauses : Parser State { command : Maybe String, subscription : Maybe String }
effectWhereClauses =
    string "where" *> Layout.layout *> whereBlock


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
        <*> (string "effect" *> Layout.layout *> moduleToken *> Layout.layout *> moduleName)
        <*> (Layout.layout *> effectWhereClauses)
        <*> exposeDefinition exposable


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    NormalModule
        <$> (succeed DefaultModuleData
                <*> (moduleToken *> Layout.layout *> moduleName)
                <*> exposeDefinition exposable
            )


portModuleDefinition : Parser State Module
portModuleDefinition =
    PortModule
        <$> (succeed DefaultModuleData
                <*> (portToken *> Layout.layout *> moduleToken *> Layout.layout *> moduleName)
                <*> exposeDefinition exposable
            )
