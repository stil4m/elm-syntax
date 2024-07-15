module Elm.Parser.Base exposing (moduleName, typeIndicator)

import Combine exposing (Parser, sepBy1Core)
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=))


moduleName : Parser state (Node ModuleName)
moduleName =
    sepBy1Core (Core.symbol ".") Tokens.typeNameCore
        |> Node.parserFromCore


typeIndicator : Parser state (Node ( ModuleName, String ))
typeIndicator =
    let
        helper : ModuleName -> String -> Core.Parser ( ModuleName, String )
        helper moduleNameSoFar typeOrSegment =
            Core.oneOf
                [ Core.succeed identity
                    |. Core.symbol "."
                    |= Tokens.typeNameCore
                    |> Core.andThen (\t -> helper (typeOrSegment :: moduleNameSoFar) t)
                , Core.succeed ()
                    |> Core.map (\() -> ( List.reverse moduleNameSoFar, typeOrSegment ))
                ]
    in
    Tokens.typeNameCore
        |> Core.andThen (\typeOrSegment -> helper [] typeOrSegment)
        |> Node.parserFromCore
