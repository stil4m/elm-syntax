module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import ParserFast


moduleName : ParserFast.Parser (Node ModuleName)
moduleName =
    ParserFast.map2 (\head tail -> head :: tail)
        Tokens.typeName
        moduleNameOrEmpty
        |> Node.parserCore


moduleNameOrEmpty : ParserFast.Parser ModuleName
moduleNameOrEmpty =
    ParserFast.orSucceed
        (ParserFast.map2 (\head tail -> head :: tail)
            (ParserFast.symbolFollowedBy "." Tokens.typeName)
            (ParserFast.lazy (\() -> moduleNameOrEmpty))
        )
        []
