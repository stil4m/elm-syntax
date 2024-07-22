module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=))


moduleName : Core.Parser (Node ModuleName)
moduleName =
    Core.succeed listCons
        |= Tokens.typeName
        |= moduleNameOrEmpty
        |> Node.parserCore


listCons : a -> List a -> List a
listCons head =
    \tail -> head :: tail


moduleNameOrEmpty : Core.Parser ModuleName
moduleNameOrEmpty =
    Core.oneOf
        [ Core.succeed listCons
            |. Tokens.dot
            |= Tokens.typeName
            |= Core.lazy (\() -> moduleNameOrEmpty)
        , Core.succeed []
        ]
