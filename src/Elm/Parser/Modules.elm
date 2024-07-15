module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing (Parser, between, oneOf, sepBy1, succeed, symbol)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, moduleToken, portToken, typeName)
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Module exposing (DefaultModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import List.Extra


moduleDefinition : Parser State Module
moduleDefinition =
    oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, Node String )
effectWhereClause =
    succeed (\fnName -> \typeName_ -> ( fnName, typeName_ ))
        |> Combine.keep functionName
        |> Combine.ignore (Layout.maybeAroundBothSides (symbol "="))
        |> Combine.keep (Node.parserFromCore typeName)


whereBlock : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
whereBlock =
    between
        (symbol "{")
        (symbol "}")
        (sepBy1 (symbol ",")
            (Layout.maybeAroundBothSides effectWhereClause)
        )
        |> Combine.map
            (\pairs ->
                { command = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "command") |> Maybe.map Tuple.second
                , subscription = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription") |> Maybe.map Tuple.second
                }
            )


effectWhereClauses : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
effectWhereClauses =
    symbol "where"
        |> Combine.continueWith Layout.layout
        |> Combine.continueWith whereBlock


effectModuleDefinition : Parser State Module
effectModuleDefinition =
    let
        createEffectModule : Node ModuleName -> { command : Maybe (Node String), subscription : Maybe (Node String) } -> Node Exposing -> Module
        createEffectModule name whereClauses exp =
            EffectModule
                { moduleName = name
                , exposingList = exp
                , command = whereClauses.command
                , subscription = whereClauses.subscription
                }
    in
    succeed (\name -> \whereClauses -> \exp -> createEffectModule name whereClauses exp)
        |> Combine.ignore (symbol "effect")
        |> Combine.ignore Layout.layout
        |> Combine.ignore moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.keep moduleName
        |> Combine.ignore Layout.layout
        |> Combine.keep effectWhereClauses
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    succeed (\moduleName -> \exposingList -> NormalModule (DefaultModuleData moduleName exposingList))
        |> Combine.ignore moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.keep moduleName
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)


portModuleDefinition : Parser State Module
portModuleDefinition =
    succeed (\moduleName -> \exposingList -> PortModule (DefaultModuleData moduleName exposingList))
        |> Combine.ignore portToken
        |> Combine.ignore Layout.layout
        |> Combine.ignore moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.keep moduleName
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)
