module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|=))


moduleName : ParserFast.Parser (Node ModuleName)
moduleName =
    ParserFast.map2 (\head tail -> head :: tail)
        Tokens.typeName
        moduleNameOrEmpty
        |> Node.parserCore


moduleNameOrEmpty : Parser.Parser ModuleName
moduleNameOrEmpty =
    Parser.oneOf
        [ Parser.map (\() -> \head -> \tail -> head :: tail) Tokens.dot
            |= Tokens.typeName
            |= Parser.lazy (\() -> moduleNameOrEmpty)
        , Parser.succeed []
        ]
