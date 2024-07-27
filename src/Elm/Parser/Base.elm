module Elm.Parser.Base exposing (moduleName)

import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))
import Parser.Extra


moduleName : Core.Parser (Node ModuleName)
moduleName =
    Core.map
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
        Core.getPosition
        |= Tokens.typeName
        |= moduleNameOrEmpty
        |= Core.getCol


listCons : a -> List a -> List a
listCons head =
    \tail -> head :: tail


moduleNameOrEmpty : Core.Parser ModuleName
moduleNameOrEmpty =
    Core.oneOf
        [ (Tokens.dot
            |> Parser.Extra.continueWith
                (Core.map listCons
                    Tokens.typeName
                )
          )
            |= Core.lazy (\() -> moduleNameOrEmpty)
        , Core.succeed []
        ]
