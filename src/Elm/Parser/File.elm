module Elm.Parser.File exposing (file)

import Elm.Parser.Comments as Comments
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Node as Node
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|=), Parser)
import ParserWithComments exposing (WithComments)
import Rope


file : Core.Parser File
file =
    Core.map
        (\commentsBeforeModuleDefinition ->
            \moduleDefinition ->
                \commentsAfterModuleDefinition ->
                    \moduleComments ->
                        \imports ->
                            \declarations ->
                                { moduleDefinition = moduleDefinition.syntax
                                , imports = imports.syntax
                                , declarations = declarations.syntax
                                , comments =
                                    Rope.flatFromList
                                        [ commentsBeforeModuleDefinition
                                        , moduleDefinition.comments
                                        , commentsAfterModuleDefinition
                                        , moduleComments
                                        , imports.comments
                                        , declarations.comments
                                        ]
                                        |> Rope.toList
                                }
        )
        Layout.layoutStrict
        |= Node.parser moduleDefinition
        |= Layout.layoutStrict
        |= Core.oneOf
            [ Core.map
                (\declarationParsed ->
                    \commentsAfter ->
                        Rope.flatFromList [ declarationParsed, commentsAfter ]
                )
                Comments.moduleDocumentation
                |= Layout.layoutStrict
            , Core.succeed Rope.empty
            ]
        |= ParserWithComments.many importDefinition
        |= fileDeclarations


fileDeclarations : Parser (WithComments (List (Node Declaration)))
fileDeclarations =
    ParserWithComments.many
        (Core.map
            (\declarationParsed ->
                \commentsAfter ->
                    { comments = Rope.flatFromList [ declarationParsed.comments, commentsAfter ]
                    , syntax = declarationParsed.syntax
                    }
            )
            declaration
            |= Core.oneOf
                [ Layout.layoutStrict
                , Core.succeed Rope.empty
                ]
        )
