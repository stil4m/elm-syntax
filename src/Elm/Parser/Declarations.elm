module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser, maybe, oneOf, string, succeed)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens exposing (functionName, portToken, prefixOperatorToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinition)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression as Expression exposing (Function, FunctionImplementation)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import List.Extra
import Parser as Core


declaration : Parser State (Node Declaration)
declaration =
    oneOf
        [ infixDeclaration
        , portDeclaration
        , maybeDocumentation
            |> Combine.andThen
                (\maybeDoc ->
                    oneOf
                        [ function maybeDoc
                        , typeDefinition maybeDoc
                        ]
                )
        ]
        |> Combine.ignore (Combine.modifyState State.parsedImportOrDeclaration)


maybeDocumentation : Parser State (Maybe (Node Documentation))
maybeDocumentation =
    Combine.oneOf
        [ Comments.declarationDocumentation
            |> Combine.ignore Layout.layoutStrict
            |> Combine.map Just
        , Combine.withState
            (\state ->
                if State.checkParsedImportOrDeclaration state then
                    Combine.succeed Nothing

                else
                    case state |> State.getComments |> List.Extra.find (\(Node _ comment) -> String.startsWith "{-|" comment) of
                        Nothing ->
                            Combine.succeed Nothing

                        Just doc ->
                            Combine.modifyState (State.removeComment doc)
                                |> Combine.continueWith (Combine.succeed (Just doc))
            )
        ]


function : Maybe (Node Documentation) -> Parser State (Node Declaration)
function maybeDoc =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map
            (\f ->
                Node
                    { start = maybeDoc |> Maybe.map (Node.range >> .start) |> Maybe.withDefault (Expression.functionRange f).start
                    , end = (Expression.functionRange f).end
                    }
                    (Declaration.FunctionDeclaration { f | documentation = maybeDoc })
            )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            succeed (\args expr -> Node { start = (Node.range varPointer).start, end = (Node.range expr).end } (FunctionImplementation varPointer args expr))
                |> Combine.keep (Combine.many (pattern |> Combine.ignore (maybe Layout.layout)))
                |> Combine.ignore (string "=")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep expression

        fromParts : Node Signature -> Node FunctionImplementation -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.ignore (maybe Layout.layoutStrict)
                |> Combine.andThen
                    (\sig ->
                        Node.parser functionName
                            |> Combine.andThen (failIfDifferentFrom varPointer)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andThen functionImplementationFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : Node String -> Parser State Function
        functionWithoutSignature varPointer =
            functionImplementationFromVarPointer varPointer
                |> Combine.map (Function Nothing Nothing)
    in
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


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
