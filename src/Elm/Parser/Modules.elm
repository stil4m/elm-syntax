module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


moduleDefinition : Parser (WithComments Module)
moduleDefinition =
    Parser.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    Parser.map
        (\fnName ->
            \commentsAfterFnName ->
                \commentsAfterEqual ->
                    \typeName_ ->
                        { comments = commentsAfterFnName |> Rope.prependTo commentsAfterEqual
                        , syntax = ( fnName, typeName_ )
                        }
        )
        Tokens.functionName
        |= Layout.maybeLayout
        |. Tokens.equal
        |= Layout.maybeLayout
        |= Node.parserCore Tokens.typeName


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    Tokens.curlyStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy1Until ","
                Tokens.curlyEnd
                (Layout.maybeAroundBothSides effectWhereClause)
                |> Parser.map
                    (\pairs ->
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
            )


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    (Tokens.whereToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBefore ->
                    \whereResult ->
                        { comments = commentsBefore |> Rope.prependTo whereResult.comments
                        , syntax = whereResult.syntax
                        }
                )
                Layout.layout
            )
    )
        |= whereBlock


effectModuleDefinition : Parser (WithComments Module)
effectModuleDefinition =
    (Parser.symbol "effect"
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterEffect ->
                    \commentsModule ->
                        \name ->
                            \commentsAfterName ->
                                \whereClauses ->
                                    \commentsAfterWhereClauses ->
                                        \exp ->
                                            { comments =
                                                commentsAfterEffect
                                                    |> Rope.prependTo commentsModule
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
                Layout.layout
            )
    )
        |. Tokens.moduleToken
        |= Layout.layout
        |= moduleName
        |= Layout.layout
        |= effectWhereClauses
        |= Layout.layout
        |= Node.parser exposeDefinition


normalModuleDefinition : Parser (WithComments Module)
normalModuleDefinition =
    (Tokens.moduleToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterModule ->
                    \moduleName ->
                        \commentsAfterModuleName ->
                            \exposingList ->
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
                Layout.layout
            )
    )
        |= moduleName
        |= Layout.layout
        |= Node.parser exposeDefinition


portModuleDefinition : Parser (WithComments Module)
portModuleDefinition =
    (Tokens.portToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterPort ->
                    \commentsAfterModule ->
                        \moduleName ->
                            \commentsAfterModuleName ->
                                \exposingList ->
                                    { comments =
                                        commentsAfterPort
                                            |> Rope.prependTo commentsAfterModule
                                            |> Rope.prependTo commentsAfterModuleName
                                            |> Rope.prependTo exposingList.comments
                                    , syntax = PortModule { moduleName = moduleName, exposingList = exposingList.syntax }
                                    }
                )
                Layout.layout
            )
    )
        |. Tokens.moduleToken
        |= Layout.layout
        |= moduleName
        |= Layout.layout
        |= Node.parser exposeDefinition
