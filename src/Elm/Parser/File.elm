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
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


file : ParserFast.Parser File
file =
    ParserFast.map5
        (\moduleDefinition moduleComments imports declarations () ->
            { moduleDefinition = moduleDefinition.syntax
            , imports = imports.syntax
            , declarations = declarations.syntax
            , comments =
                moduleDefinition.comments
                    |> Rope.prependTo moduleComments
                    |> Rope.prependTo imports.comments
                    |> Rope.prependTo declarations.comments
                    |> Rope.toList
            }
        )
        (Layout.layoutStrictFollowedByWithComments
            (Node.parser moduleDefinition)
        )
        (Layout.layoutStrictFollowedByComments
            (ParserFast.orSucceed
                (ParserFast.map2
                    (\moduleDocumentation commentsAfter ->
                        Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                    )
                    Comments.moduleDocumentation
                    Layout.layoutStrict
                )
                Rope.empty
            )
        )
        (ParserWithComments.many importDefinition)
        fileDeclarations
        ParserFast.end


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
        )
