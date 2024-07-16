module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=))
import Parser.Extra


moduleName : Core.Parser (Node ModuleName)
moduleName =
    Parser.Extra.sepBy1 "." Tokens.typeName
        |> Node.parserCore


typeIndicator : Core.Parser (Node ( ModuleName, String ))
typeIndicator =
    let
        helper : ModuleName -> String -> Core.Parser ( ModuleName, String )
        helper moduleNameSoFar typeOrSegment =
            Core.oneOf
                [ Core.succeed identity
                    |. Core.symbol "."
                    |= Tokens.typeName
                    |> Core.andThen (\t -> helper (typeOrSegment :: moduleNameSoFar) t)
                , Core.succeed ()
                    |> Core.map (\() -> ( List.reverse moduleNameSoFar, typeOrSegment ))
                ]
    in
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> helper [] typeOrSegment)
        |> Node.parserCore
