module Elm.Parser.Declarations exposing (declaration, function, signature)

import Combine exposing (Parser, choice, maybe, string, succeed)
import Elm.Parser.Expression
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges as Ranges
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, portToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDefinition)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Port exposing (Port)
import Elm.Syntax.Range as Range
import Elm.Syntax.Signature exposing (Signature)


declaration : Parser State (Node Declaration)
declaration =
    choice
        [ infixDeclaration
        , function
        , typeDefinition
            |> Combine.map
                (\v ->
                    case v of
                        Typings.DefinedType r t ->
                            Node r (Declaration.CustomTypeDeclaration t)

                        Typings.DefinedAlias r a ->
                            Node r (Declaration.AliasDeclaration a)
                )
        , portDeclaration
        ]


function : Parser State (Node Declaration)
function =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen Elm.Parser.Expression.functionWithNameNode
        |> Combine.map (\f -> Node (Expression.functionRange f) (Declaration.FunctionDeclaration f))


signature : Parser State Signature
signature =
    succeed Signature
        |> Combine.andMap (Node.parser functionName)
        |> Combine.andMap (Layout.maybeAroundBothSides (string ":") |> Combine.continueWith (maybe Layout.layout) |> Combine.continueWith typeAnnotation)


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            Infix.infixDefinition
                |> Combine.map (\inf -> Node (Range.combine [ current, Node.range inf.function ]) (Declaration.InfixDeclaration inf))
        )


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            portToken
                |> Combine.ignore Layout.layout
                |> Combine.continueWith (Node.parser signature)
                |> Combine.map
                    (\sig ->
                        Node
                            (Range.combine
                                [ current, (\(Node r _) -> r) sig ]
                            )
                            (Declaration.PortDeclaration (Port Nothing sig))
                    )
        )
