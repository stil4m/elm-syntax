module Elm.Parser.File exposing (file)

import CustomParser exposing (Parser )
import Elm.Parser.Comments as Comments
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Node as Node
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import ParserWithComments exposing (WithComments)
import Rope


file : CustomParser.Parser File
file =
    CustomParser.map
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
        |> CustomParser.keep (Node.parser moduleDefinition)
        |> CustomParser.keep Layout.layoutStrict
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\moduleDocumentation ->
                        \commentsAfter ->
                            Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                    )
                    Comments.moduleDocumentation
                    |> CustomParser.keep Layout.layoutStrict
                , CustomParser.succeed Rope.empty
                ]
            )
        |> CustomParser.keep (ParserWithComments.many importDefinition)
        |> CustomParser.keep fileDeclarations
        |> CustomParser.ignore CustomParser.end


fileDeclarations : Parser (WithComments (List (Node Declaration)))
fileDeclarations =
    ParserWithComments.many
        (Layout.moduleLevelIndentationFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> Rope.prependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                declaration
                Layout.optimisticLayout
            )
            |> CustomParser.keep declaration
            |> CustomParser.keep Layout.optimisticLayout
        )
