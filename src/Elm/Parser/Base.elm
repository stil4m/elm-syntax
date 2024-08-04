module Elm.Parser.Base exposing (moduleName)

import CustomParser
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))


moduleName : CustomParser.Parser (Node ModuleName)
moduleName =
    CustomParser.map4
        (\start head tail endColumn ->
            Node
                { start = start
                , end = { row = start.row, column = endColumn }
                }
                (head :: tail)
        )
        CustomParser.getPosition
        Tokens.typeName
        moduleNameOrEmpty
        CustomParser.getCol


moduleNameOrEmpty : CustomParser.Parser ModuleName
moduleNameOrEmpty =
    CustomParser.oneOf
        [ CustomParser.map3 (\() head tail -> head :: tail)
            Tokens.dot
            Tokens.typeName
            (CustomParser.lazy (\() -> moduleNameOrEmpty))
        , CustomParser.succeed []
        ]
