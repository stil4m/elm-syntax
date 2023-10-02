module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser, maybe, oneOf, string, succeed)
import Elm.Parser.Expression exposing (functionWithNameNode)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, portToken, prefixOperatorToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinition)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core


declaration : Parser State (Node Declaration)
declaration =
    oneOf
        [ infixDeclaration
        , function
        , typeDefinition
        , portDeclaration
        ]


function : Parser State (Node Declaration)
function =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map (\f -> Node (Expression.functionRange f) (Declaration.FunctionDeclaration f))


signature : Parser State Signature
signature =
    succeed Signature
        |> Combine.keep (Node.parser functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (string ":"))
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep typeAnnotation


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    succeed Infix
        |> Combine.ignore (Combine.fromCore (Core.keyword "infix"))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser (Combine.fromCore Core.int))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser <| Combine.parens prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser functionName)
        |> Combine.map Declaration.InfixDeclaration
        |> Node.parser


infixDirection : Parser State Infix.InfixDirection
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\_ -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\_ -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\_ -> Infix.Non)
        ]
        |> Combine.fromCore


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Combine.succeed
        (\(Node { start } _) sig ->
            Node
                { start = start, end = (Node.range sig.typeAnnotation).end }
                (Declaration.PortDeclaration sig)
        )
        |> Combine.keep (Node.parser portToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep signature
