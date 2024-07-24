module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import List.Extra
import Parser as Core


moduleDefinition : Parser State Module
moduleDefinition =
    Combine.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, Node String )
effectWhereClause =
    Core.map (\fnName -> \typeName_ -> ( fnName, typeName_ ))
        Tokens.functionName
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keepFromCore (Node.parserCore Tokens.typeName)


whereBlock : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
whereBlock =
    Combine.betweenMap
        (\pairs ->
            { command = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "command") |> Maybe.map Tuple.second
            , subscription = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription") |> Maybe.map Tuple.second
            }
        )
        Tokens.curlyStart
        Tokens.curlyEnd
        (Combine.sepBy1 ","
            (Layout.maybeAroundBothSides effectWhereClause)
        )


effectWhereClauses : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
effectWhereClauses =
    Tokens.whereToken
        |> Combine.fromCoreIgnore Layout.layout
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
    Core.symbol "effect"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.ignoreEntirely Tokens.moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.continueWithCore
            (Core.map (\name -> \whereClauses -> \exp -> createEffectModule name whereClauses exp)
                moduleName
            )
        |> Combine.ignore Layout.layout
        |> Combine.keep effectWhereClauses
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    Tokens.moduleToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.continueWithCore
            (Core.map
                (\moduleName ->
                    \exposingList ->
                        NormalModule { moduleName = moduleName, exposingList = exposingList }
                )
                moduleName
            )
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)


portModuleDefinition : Parser State Module
portModuleDefinition =
    Tokens.portToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.ignoreEntirely Tokens.moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.continueWithCore
            (Core.map (\moduleName -> \exposingList -> PortModule { moduleName = moduleName, exposingList = exposingList })
                moduleName
            )
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser exposeDefinition)
