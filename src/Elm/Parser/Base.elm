module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|=))


moduleName : Parser.Parser (Node ModuleName)
moduleName =
    Parser.map
        (\( startRow, startColumn ) ->
            \head ->
                \tail ->
                    \endColumn ->
                        Node
                            { start = { row = startRow, column = startColumn }
                            , end = { row = startRow, column = endColumn }
                            }
                            (head :: tail)
        )
        Parser.getPosition
        |= Tokens.typeName
        |= moduleNameOrEmpty
        |= Parser.getCol


moduleNameOrEmpty : Parser.Parser ModuleName
moduleNameOrEmpty =
    Parser.oneOf
        [ Parser.map (\() -> \head -> \tail -> head :: tail) Tokens.dot
            |= Tokens.typeName
            |= Parser.lazy (\() -> moduleNameOrEmpty)
        , Parser.succeed []
        ]
