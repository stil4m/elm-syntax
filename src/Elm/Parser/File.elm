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
                                        [ commentsBeforeModuleDefinition.comments
                                        , moduleDefinition.comments
                                        , commentsAfterModuleDefinition.comments
                                        , moduleComments.comments
                                        , imports.comments
                                        , declarations.comments
                                        ]
                                        |> Rope.toList
                                }
        )
        Layout.layoutStrict
        |= Node.parser moduleDefinition
        |= Layout.layoutStrict
        |= ParserWithComments.maybeIgnore
            (Comments.moduleDocumentation
                |> ParserWithComments.ignore Layout.layoutStrict
            )
        |= ParserWithComments.many importDefinition
        |= fileDeclarations


fileDeclarations : Parser (WithComments (List (Node Declaration)))
fileDeclarations =
    ParserWithComments.many
        (declaration
            |> ParserWithComments.ignore (ParserWithComments.maybeIgnore Layout.layoutStrict)
        )
