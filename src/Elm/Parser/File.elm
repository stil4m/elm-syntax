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
import Parser exposing ((|.), (|=), Parser)
import ParserWithComments exposing (WithComments)
import Rope


file : Parser.Parser File
file =
    Parser.map
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
                                    commentsBeforeModuleDefinition
                                        |> Rope.prependTo moduleDefinition.comments
                                        |> Rope.prependTo commentsAfterModuleDefinition
                                        |> Rope.prependTo moduleComments
                                        |> Rope.prependTo imports.comments
                                        |> Rope.prependTo declarations.comments
                                        |> Rope.toList
                                }
        )
        Layout.layoutStrict
        |= Node.parser moduleDefinition
        |= Layout.layoutStrict
        |= Parser.oneOf
            [ Parser.map
                (\moduleDocumentation ->
                    \commentsAfter ->
                        Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                )
                Comments.moduleDocumentation
                |= Layout.layoutStrict
            , Parser.succeed Rope.empty
            ]
        |= ParserWithComments.many importDefinition
        |= fileDeclarations
        |. Parser.end


fileDeclarations : Parser (WithComments (List (Node Declaration)))
fileDeclarations =
    ParserWithComments.many
        (Parser.map
            (\declarationParsed ->
                \commentsAfter ->
                    { comments = declarationParsed.comments |> Rope.prependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
            )
            declaration
            |= Parser.oneOf
                [ Layout.layoutStrict
                , Parser.succeed Rope.empty
                ]
        )
