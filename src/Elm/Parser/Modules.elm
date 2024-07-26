module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node)
import List.Extra
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


moduleDefinition : Parser (WithComments Module)
moduleDefinition =
    Core.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    (Core.map
        (\fnName ->
            \commentsAfterFnName ->
                \commentsAfterEqual ->
                    \typeName_ ->
                        { comments = Rope.flatFromList [ commentsAfterFnName, commentsAfterEqual ]
                        , syntax = ( fnName, typeName_ )
                        }
        )
        Tokens.functionName
        |= Layout.maybeLayout
    )
        |. Tokens.equal
        |= Layout.maybeLayout
        |= Node.parserCore Tokens.typeName


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy1 ","
                (Layout.maybeAroundBothSides effectWhereClause)
                |> Core.map
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
    )
        |. Tokens.curlyEnd


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    (Tokens.whereToken
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBefore ->
                    \whereResult ->
                        { comments = Rope.flatFromList [ commentsBefore, whereResult.comments ]
                        , syntax = whereResult.syntax
                        }
                )
                Layout.layout
            )
    )
        |= whereBlock


effectModuleDefinition : Parser (WithComments Module)
effectModuleDefinition =
    (Core.symbol "effect"
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsAfterEffect ->
                    \commentsModule ->
                        \name ->
                            \commentsAfterName ->
                                \whereClauses ->
                                    \commentsAfterWhereClauses ->
                                        \exp ->
                                            { comments =
                                                Rope.flatFromList
                                                    [ commentsAfterEffect
                                                    , commentsModule
                                                    , commentsAfterName
                                                    , whereClauses.comments
                                                    , commentsAfterWhereClauses
                                                    , exp.comments
                                                    ]
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
            (Core.map
                (\commentsAfterModule ->
                    \moduleName ->
                        \commentsAfterModuleName ->
                            \exposingList ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterModule
                                        , commentsAfterModuleName
                                        , exposingList.comments
                                        ]
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
            (Core.map
                (\commentsAfterPort ->
                    \commentsAfterModule ->
                        \moduleName ->
                            \commentsAfterModuleName ->
                                \exposingList ->
                                    { comments =
                                        Rope.flatFromList
                                            [ commentsAfterPort
                                            , commentsAfterModule
                                            , commentsAfterModuleName
                                            , exposingList.comments
                                            ]
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
