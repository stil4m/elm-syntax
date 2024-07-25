module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import List.Extra
import Parser as Core exposing ((|.))
import Parser.Extra
import ParserWithComments exposing (ParserWithComments)


moduleDefinition : ParserWithComments Module
moduleDefinition =
    Core.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : ParserWithComments ( String, Node String )
effectWhereClause =
    (Core.map (\fnName -> \typeName_ -> ( fnName, typeName_ ))
        Tokens.functionName
        |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keepFromCore (Node.parserCore Tokens.typeName)


whereBlock : ParserWithComments { command : Maybe (Node String), subscription : Maybe (Node String) }
whereBlock =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy1 ","
                (Layout.maybeAroundBothSides effectWhereClause)
                |> ParserWithComments.map
                    (\pairs ->
                        { command = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "command") |> Maybe.map Tuple.second
                        , subscription = pairs |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription") |> Maybe.map Tuple.second
                        }
                    )
            )
    )
        |. Tokens.curlyEnd


effectWhereClauses : ParserWithComments { command : Maybe (Node String), subscription : Maybe (Node String) }
effectWhereClauses =
    Tokens.whereToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.continueWith whereBlock


effectModuleDefinition : ParserWithComments Module
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
    (Core.symbol "effect"
        |> ParserWithComments.fromCoreIgnore Layout.layout
    )
        |. Tokens.moduleToken
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.continueWithCore
            (Core.map (\name -> \whereClauses -> \exp -> createEffectModule name whereClauses exp)
                moduleName
            )
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keep effectWhereClauses
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keep (Node.parser exposeDefinition)


normalModuleDefinition : ParserWithComments Module
normalModuleDefinition =
    Tokens.moduleToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.continueWithCore
            (Core.map
                (\moduleName ->
                    \exposingList ->
                        NormalModule { moduleName = moduleName, exposingList = exposingList }
                )
                moduleName
            )
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keep (Node.parser exposeDefinition)


portModuleDefinition : ParserWithComments Module
portModuleDefinition =
    (Tokens.portToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
    )
        |. Tokens.moduleToken
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.continueWithCore
            (Core.map (\moduleName -> \exposingList -> PortModule { moduleName = moduleName, exposingList = exposingList })
                moduleName
            )
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keep (Node.parser exposeDefinition)
