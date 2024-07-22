module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (customTypeDefinitionWithoutDocumentation, typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix, typeDefinitionAfterDocumentation)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|.), (|=))


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ Comments.declarationDocumentation
            |> Combine.ignoreFromCore Layout.layoutStrict
            |> Combine.andThen
                (\documentation ->
                    Combine.oneOf
                        [ functionAfterDocumentation documentation
                        , typeDefinitionAfterDocumentation documentation
                        , portDeclaration documentation
                        ]
                )
        , infixDeclaration

        -- if there is no type annotation (which should be rather rare in practice),
        -- this will parse the name + maybe layout twice.
        -- However, this allows us to pre-define this parser without needing `andThen`,
        -- which does make up for it (if you don't have very long function names or big comments between the name and the `:`)
        , functionDeclarationWithoutDocumentationWithSignatureWithNameAndMaybeLayoutBacktrackable

        -- Unlike typeDefinitionAfterDocumentation, we _need_ the position before the `type` token,
        -- so without using backtrackable, we would need an `andThen` to construct the rest of the parser
        -- which is bad for performance.
        -- The current tradeoff does indeed perform worse when there's a big comment between `type` and `alias`.
        -- However, this seems incredibly rare in practice.
        , typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix
        , customTypeDefinitionWithoutDocumentation

        --
        , functionDeclarationWithoutDocumentationWithoutSignature
        , portDeclarationWithoutDocumentation
        ]


functionAfterDocumentation : Node Documentation -> Parser State (Node Declaration)
functionAfterDocumentation documentation =
    functionNameMaybeLayout
        |> Combine.andThen
            (\startName ->
                functionWithNameNode (Node.range documentation).start startName (Just documentation)
            )


functionWithNameNode : Location -> Node String -> Maybe (Node String) -> Parser State (Node Declaration)
functionWithNameNode start ((Node _ startName) as startNameNode) maybeDocumentation =
    Combine.oneOf
        [ Combine.succeed
            (\typeAnnotation ->
                \((Node implementationNameRange _) as implementationName) ->
                    \arguments ->
                        \((Node { end } _) as expression) ->
                            Node { start = start, end = end }
                                (Declaration.FunctionDeclaration
                                    { documentation = maybeDocumentation
                                    , signature = Just (Node.combine Signature startNameNode typeAnnotation)
                                    , declaration =
                                        Node { start = implementationNameRange.start, end = end }
                                            (FunctionImplementation implementationName arguments expression)
                                    }
                                )
            )
            |> Combine.ignore colonMaybeLayout
            |> Combine.keep typeAnnotationLayout
            |> Combine.keep
                (functionNameMaybeLayout
                    |> Combine.andThen
                        (\implementationName ->
                            if Node.value implementationName == startName then
                                Combine.succeed implementationName

                            else
                                Combine.problem
                                    ("Expected to find the declaration for " ++ startName ++ " but found " ++ Node.value implementationName)
                        )
                )
            |> Combine.keep patternListEqualsMaybeLayout
            |> Combine.keep expression
        , Combine.succeed
            (\args ->
                \((Node { end } _) as expression) ->
                    Node { start = start, end = end }
                        (Declaration.FunctionDeclaration
                            { documentation = maybeDocumentation
                            , signature = Nothing
                            , declaration =
                                Node { start = (Node.range startNameNode).start, end = end }
                                    (FunctionImplementation startNameNode args expression)
                            }
                        )
            )
            |> Combine.keep patternListEqualsMaybeLayout
            |> Combine.keep expression
        ]


functionDeclarationWithoutDocumentationWithSignatureWithNameAndMaybeLayoutBacktrackable : Parser State (Node Declaration)
functionDeclarationWithoutDocumentationWithSignatureWithNameAndMaybeLayoutBacktrackable =
    Combine.succeed
        (\((Node { start } _) as startNameNode) ->
            \typeAnnotation ->
                \((Node implementationNameRange _) as implementationName) ->
                    \arguments ->
                        \((Node { end } _) as result) ->
                            if Node.value implementationName == Node.value startNameNode then
                                Combine.succeed
                                    (Node { start = start, end = end }
                                        (FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Just (Node.combine Signature startNameNode typeAnnotation)
                                            , declaration =
                                                Node { start = implementationNameRange.start, end = end }
                                                    (FunctionImplementation implementationName arguments result)
                                            }
                                        )
                                    )

                            else
                                Combine.problem
                                    ("Expected to find the declaration for " ++ Node.value startNameNode ++ " but found " ++ Node.value implementationName)
        )
        |> Combine.keep
            (functionNameMaybeLayout
                |> Combine.backtrackable
            )
        |> Combine.ignore colonMaybeLayout
        |> Combine.keep typeAnnotationLayout
        |> Combine.keep functionNameMaybeLayout
        |> Combine.keep patternListEqualsMaybeLayout
        |> Combine.keep expression
        |> Combine.andThen identity


functionDeclarationWithoutDocumentationWithoutSignature : Parser State (Node Declaration)
functionDeclarationWithoutDocumentationWithoutSignature =
    Combine.succeed
        (\((Node { start } _) as startNameNode) ->
            \args ->
                \((Node { end } _) as result) ->
                    Node { start = start, end = end }
                        (FunctionDeclaration
                            { documentation = Nothing
                            , signature = Nothing
                            , declaration =
                                Node { start = start, end = end }
                                    (FunctionImplementation startNameNode args result)
                            }
                        )
        )
        |> Combine.keep functionNameMaybeLayout
        |> Combine.keep patternListEqualsMaybeLayout
        |> Combine.keep expression


functionNameMaybeLayout : Parser State (Node String)
functionNameMaybeLayout =
    Tokens.functionName
        |> Node.parserCore
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)


colonMaybeLayout : Parser State ()
colonMaybeLayout =
    Tokens.colon
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)


typeAnnotationLayout : Parser State (Node TypeAnnotation)
typeAnnotationLayout =
    TypeAnnotation.typeAnnotation
        |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)


patternListEqualsMaybeLayout : Parser State (List (Node Pattern))
patternListEqualsMaybeLayout =
    Combine.many (pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Combine.succeed
        (\direction ->
            \precedence ->
                \operator ->
                    \fn ->
                        Declaration.InfixDeclaration
                            { direction = direction, precedence = precedence, operator = operator, function = fn }
        )
        |> Combine.ignoreEntirely (Core.keyword "infix")
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore infixDirection
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Core.int)
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore operatorWithParens
        |> Combine.ignore Layout.layout
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Node.parser


operatorWithParens : Core.Parser (Node String)
operatorWithParens =
    Core.succeed identity
        |. Tokens.parensStart
        |= Tokens.prefixOperatorToken
        |. Tokens.parensEnd
        |> Node.parserCore


infixDirection : Core.Parser (Node Infix.InfixDirection)
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


portDeclaration : Node Documentation -> Parser State (Node Declaration)
portDeclaration documentation =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \name ->
                \((Node { end } _) as typeAnnotation) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = end
                        }
                        (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotation })
        )
        |> Combine.ignore
            (Combine.modifyState (State.addComment documentation))
        |> Combine.keep getPositionPortTokenLayout
        |> Combine.keep functionNameLayoutColonLayout
        |> Combine.keep typeAnnotation


functionNameLayoutColonLayout : Parser State (Node String)
functionNameLayoutColonLayout =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Layout.maybeAroundBothSides (Combine.symbol ":"))


getPositionPortTokenLayout : Parser State ( Int, Int )
getPositionPortTokenLayout =
    Core.getPosition
        |. Tokens.portToken
        |> Combine.ignoreFromCore Layout.layout


portDeclarationWithoutDocumentation : Parser State (Node Declaration)
portDeclarationWithoutDocumentation =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \name ->
                \((Node { end } _) as typeAnnotation) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = end
                        }
                        (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotation })
        )
        |> Combine.keepFromCore Core.getPosition
        |> Combine.ignoreEntirely Tokens.portToken
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.symbol ":"))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation
