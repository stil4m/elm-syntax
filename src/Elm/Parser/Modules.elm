module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node)
import List.Extra
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


moduleDefinition : Parser (WithComments Module)
moduleDefinition =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual typeName_ ->
            { comments = commentsAfterFnName |> Rope.prependTo commentsAfterEqual
            , syntax = ( fnName, typeName_ )
            }
        )
        Tokens.functionName
        (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy "=")
        Layout.maybeLayout
        (Node.parserCore Tokens.typeName)


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    ParserFast.map2
        (\pairs () ->
            { comments = pairs.comments
            , syntax =
                { command =
                    pairs.syntax
                        |> List.Extra.find (\( fnName, _ ) -> fnName == "command")
                        |> Maybe.map Tuple.second
                , subscription =
                    pairs.syntax
                        |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription")
                        |> Maybe.map Tuple.second
                }
            }
        )
        (ParserFast.symbolFollowedBy "{"
            (ParserWithComments.sepBy1
                ","
                (Layout.maybeAroundBothSides effectWhereClause)
            )
        )
        Tokens.curlyEnd


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> Rope.prependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" Layout.maybeLayout)
        whereBlock


effectModuleDefinition : Parser (WithComments Module)
effectModuleDefinition =
    ParserFast.map7
        (\commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            { comments =
                commentsAfterEffect
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo whereClauses.comments
                    |> Rope.prependTo commentsAfterWhereClauses
                    |> Rope.prependTo exp.comments
            , syntax =
                EffectModule
                    { moduleName = name
                    , exposingList = exp.syntax
                    , command = whereClauses.syntax.command
                    , subscription = whereClauses.syntax.subscription
                    }
            }
        )
        (ParserFast.keywordFollowedBy "effect" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        effectWhereClauses
        Layout.maybeLayout
        (Node.parser exposeDefinition)


normalModuleDefinition : Parser (WithComments Module)
normalModuleDefinition =
    ParserFast.map4
        (\commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                NormalModule
                    { moduleName = moduleName
                    , exposingList = exposingList.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        (Node.parser exposeDefinition)


portModuleDefinition : Parser (WithComments Module)
portModuleDefinition =
    ParserFast.map5
        (\commentsAfterPort commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax = PortModule { moduleName = moduleName, exposingList = exposingList.syntax }
            }
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        moduleName
        Layout.maybeLayout
        (Node.parser exposeDefinition)
