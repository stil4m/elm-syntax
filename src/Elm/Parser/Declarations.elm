module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinition)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression as Expression exposing (Function, FunctionImplementation)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|.), (|=))
import Parser.Extra


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ infixDeclaration
        , maybeDocumentation
            |> Combine.andThen
                (\maybeDoc ->
                    Combine.oneOf
                        [ function maybeDoc
                        , typeDefinition maybeDoc
                        , portDeclaration maybeDoc
                        ]
                )
        ]


maybeDocumentation : Parser State (Maybe (Node Documentation))
maybeDocumentation =
    Comments.declarationDocumentation
        |> Combine.ignoreFromCore Layout.layoutStrict
        |> Combine.maybe


function : Maybe (Node Documentation) -> Parser State (Node Declaration)
function maybeDoc =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map
            (\f ->
                let
                    ({ end } as functionRange) =
                        Expression.functionRange f
                in
                case maybeDoc of
                    Just (Node { start } _) ->
                        Node { start = start, end = end } (Declaration.FunctionDeclaration { f | documentation = maybeDoc })

                    Nothing ->
                        Node functionRange (Declaration.FunctionDeclaration f)
            )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    let
        functionImplementationFromVarPointer : Maybe (Node Signature) -> Node String -> Parser State Function
        functionImplementationFromVarPointer signature_ ((Node { start } _) as varPointer) =
            Combine.succeed
                (\args ((Node { end } _) as expr) ->
                    { documentation = Nothing
                    , signature = signature_
                    , declaration =
                        Node { start = start, end = end }
                            (FunctionImplementation varPointer args expr)
                    }
                )
                |> Combine.keep (Combine.many (pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
                |> Combine.ignoreEntirely (Core.symbol "=")
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                |> Combine.keep expression

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
                |> Combine.andThen
                    (\sig ->
                        Node.parserFromCore Tokens.functionName
                            |> Combine.andThen (\fnName -> failIfDifferentFrom varPointer fnName)
                            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                            |> Combine.andThen (\body -> functionImplementationFromVarPointer (Just sig) body)
                    )

        functionWithoutSignature : Node String -> Parser State Function
        functionWithoutSignature varPointer =
            functionImplementationFromVarPointer Nothing varPointer
    in
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


signature : Parser State Signature
signature =
    Combine.succeed (\name -> \typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.symbol ":"))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Combine.succeed (\direction -> \precedence -> \operator -> \fn -> { direction = direction, precedence = precedence, operator = operator, function = fn })
        |> Combine.ignoreEntirely (Core.keyword "infix")
        |> Combine.ignore Layout.layout
        |> Combine.keep infixDirection
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Core.int)
        |> Combine.ignore Layout.layout
        |> Combine.keep operatorWithParens
        |> Combine.ignore Layout.layout
        |> Combine.ignoreEntirely (Core.symbol "=")
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.map Declaration.InfixDeclaration
        |> Node.parser


operatorWithParens : Parser state (Node String)
operatorWithParens =
    Core.succeed identity
        |. Core.symbol "("
        |= Tokens.prefixOperatorToken
        |. Core.symbol ")"
        |> Node.parserCore
        |> Combine.fromCore


infixDirection : Parser State (Node Infix.InfixDirection)
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\() -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\() -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\() -> Infix.Non)
        ]
        |> Node.parserCore
        |> Combine.fromCore


portDeclaration : Maybe (Node Documentation) -> Parser State (Node Declaration)
portDeclaration maybeDoc =
    Combine.succeed
        (\start ->
            \sig ->
                Node
                    { start = start, end = (Node.range sig.typeAnnotation).end }
                    (Declaration.PortDeclaration sig)
        )
        |> Combine.ignore
            (case maybeDoc of
                Nothing ->
                    Combine.succeed ()

                Just doc ->
                    Combine.modifyState (State.addComment doc)
            )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.portToken
        |> Combine.ignore Layout.layout
        |> Combine.keep signature
