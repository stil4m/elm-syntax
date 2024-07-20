module Elm.Parser.Base exposing (moduleName, typeIndicator)

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


typeIndicator : Core.Parser (Node ( ModuleName, String ))
typeIndicator =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> typeIndicatorHelper [] typeOrSegment)
        |> Node.parserCore


typeIndicatorHelper : ModuleName -> String -> Core.Parser ( ModuleName, String )
typeIndicatorHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ Core.succeed identity
            |. Tokens.dot
            |= Tokens.typeName
            |> Core.andThen (\t -> typeIndicatorHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed ( List.reverse moduleNameSoFar, typeOrSegment ))
        ]
