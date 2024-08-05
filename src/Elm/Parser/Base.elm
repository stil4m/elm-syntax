module Elm.Parser.Base exposing (moduleName)

import CustomParser
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


moduleName : CustomParser.Parser (Node ModuleName)
moduleName =
    CustomParser.map2 (\head tail -> head :: tail)
        Tokens.typeName
        moduleNameOrEmpty
        |> Node.parserCore


moduleNameOrEmpty : CustomParser.Parser ModuleName
moduleNameOrEmpty =
    CustomParser.orSucceed
        (CustomParser.map2 (\head tail -> head :: tail)
            (CustomParser.symbolFollowedBy "." Tokens.typeName)
            (CustomParser.lazy (\() -> moduleNameOrEmpty))
        )
        []
