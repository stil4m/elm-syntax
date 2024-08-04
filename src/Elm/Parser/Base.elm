module Elm.Parser.Base exposing (moduleName)

import CustomParser
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))


moduleName : CustomParser.Parser (Node ModuleName)
moduleName =
    CustomParser.map
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
        CustomParser.getPosition
        |> CustomParser.keep Tokens.typeName
        |> CustomParser.keep moduleNameOrEmpty
        |> CustomParser.keep CustomParser.getCol


moduleNameOrEmpty : CustomParser.Parser ModuleName
moduleNameOrEmpty =
    CustomParser.oneOf
        [ CustomParser.map (\() -> \head -> \tail -> head :: tail) Tokens.dot
            |> CustomParser.keep Tokens.typeName
            |> CustomParser.keep (CustomParser.lazy (\() -> moduleNameOrEmpty))
        , CustomParser.succeed []
        ]
