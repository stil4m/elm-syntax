module Elm.Parser.Signature exposing (signature)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)


signature : Parser State (Node Signature)
signature =
    Combine.succeed
        (\((Node { start } _) as name) ->
            \((Node { end } _) as typeAnnotation) ->
                Node { start = start, end = end } { name = name, typeAnnotation = typeAnnotation }
        )
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.symbol ":"))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation
