module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Parser as Core exposing ((|.), (|=))


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ Core.map (\documentation -> \withDocumentation -> withDocumentation documentation)
            Comments.declarationDocumentation
            |> Combine.fromCoreIgnore Layout.layoutStrict
            |> Combine.keep
                (Combine.oneOf
                    [ functionAfterDocumentation
                    , typeOrTypeAliasDefinitionAfterDocumentation
                    , portDeclarationAfterDocumentation
                    ]
                )
            |> Combine.andThen identity
        , infixDeclaration
        , functionDeclarationWithoutDocumentation
        , typeOrTypeAliasDefinitionWithoutDocumentation
        , portDeclarationWithoutDocumentation
        ]


functionAfterDocumentation : Parser State (Node Documentation -> Parser State (Node Declaration))
functionAfterDocumentation =
    Node.parserCoreMap
        (\startName ->
            \fromStartStartNameDocumentation ->
                \documentation ->
                    fromStartStartNameDocumentation (Node.range documentation).start startName (Just documentation)
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep functionDeclarationWith


functionDeclarationWith : Parser State (Location -> Node String -> Maybe (Node String) -> Parser State (Node Declaration))
functionDeclarationWith =
    Combine.oneOf
        [ Tokens.colon
            |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
            |> Combine.continueWith
                (Combine.map
                    (\typeAnnotation ->
                        \((Node implementationNameRange implementationName) as implementationNameNode) ->
                            \arguments ->
                                \((Node { end } _) as expression) ->
                                    \start ((Node _ startName) as startNameNode) maybeDocumentation ->
                                        if implementationName == startName then
                                            Combine.succeed
                                                (Node { start = start, end = end }
                                                    (Declaration.FunctionDeclaration
                                                        { documentation = maybeDocumentation
                                                        , signature = Just (Node.combine Signature startNameNode typeAnnotation)
                                                        , declaration =
                                                            Node { start = implementationNameRange.start, end = end }
                                                                { name = implementationNameNode, arguments = arguments, expression = expression }
                                                        }
                                                    )
                                                )

                                        else
                                            Combine.problem
                                                ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
                    )
                    TypeAnnotation.typeAnnotation
                )
            |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
            |> Combine.keepFromCore (Tokens.functionName |> Node.parserCore)
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            |> Combine.keep
                (Combine.many (pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
            |> Combine.ignoreEntirely Tokens.equal
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            |> Combine.keep expression
        , Combine.map
            (\args ->
                \((Node { end } _) as expression) ->
                    \start startNameNode maybeDocumentation ->
                        Node { start = start, end = end }
                            (Declaration.FunctionDeclaration
                                { documentation = maybeDocumentation
                                , signature = Nothing
                                , declaration =
                                    Node { start = (Node.range startNameNode).start, end = end }
                                        { name = startNameNode, arguments = args, expression = expression }
                                }
                            )
                            |> Combine.succeed
            )
            (Combine.many (pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
            |> Combine.ignoreEntirely Tokens.equal
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            |> Combine.keep expression
        ]


functionDeclarationWithoutDocumentation : Parser State (Node Declaration)
functionDeclarationWithoutDocumentation =
    Node.parserCoreMap
        (\((Node startNameRange _) as startName) ->
            \fromStartStartNameDocumentation ->
                fromStartStartNameDocumentation startNameRange.start startName Nothing
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep functionDeclarationWith
        |> Combine.andThen identity


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Core.map
        (\( startRow, startColumn ) ->
            \direction ->
                \precedence ->
                    \operator ->
                        \((Node fnRange _) as fn) ->
                            Node
                                { start = { row = startRow, column = startColumn }
                                , end = fnRange.end
                                }
                                (Declaration.InfixDeclaration
                                    { direction = direction, precedence = precedence, operator = operator, function = fn }
                                )
        )
        Core.getPosition
        |. Core.keyword "infix"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keepFromCore (Node.parserCore infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Core.int)
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore operatorWithParens
        |> Combine.ignore Layout.layout
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)


operatorWithParens : Core.Parser (Node String)
operatorWithParens =
    Core.map (\() -> identity) Tokens.parensStart
        |= Tokens.prefixOperatorToken
        |. Tokens.parensEnd
        |> Node.parserCore


infixDirection : Core.Parser Infix.InfixDirection
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\() -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\() -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\() -> Infix.Non)
        ]


portDeclarationAfterDocumentation : Parser State (Node Documentation -> Parser State (Node Declaration))
portDeclarationAfterDocumentation =
    -- we have to construct the whole parser inside succeed because we need to guarantee that the comment
    -- order is preserved
    Combine.succeed
        (\documentation ->
            Combine.map
                (\() ->
                    \( startRow, startColumn ) ->
                        \name ->
                            \((Node { end } _) as typeAnnotation) ->
                                Node
                                    { start = { row = startRow, column = startColumn }
                                    , end = end
                                    }
                                    (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotation })
                )
                (Combine.modifyState (State.addComment documentation))
                |> Combine.keep getPositionPortTokenLayout
                |> Combine.keep functionNameLayoutColonLayout
                |> Combine.keep typeAnnotation
        )


functionNameLayoutColonLayout : Parser State (Node String)
functionNameLayoutColonLayout =
    Node.parserCore Tokens.functionName
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely (Core.symbol ":")
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)


getPositionPortTokenLayout : Parser State ( Int, Int )
getPositionPortTokenLayout =
    Core.getPosition
        |. Tokens.portToken
        |> Combine.fromCoreIgnore Layout.layout


portDeclarationWithoutDocumentation : Parser State (Node Declaration)
portDeclarationWithoutDocumentation =
    Core.map
        (\( startRow, startColumn ) ->
            \name ->
                \((Node { end } _) as typeAnnotation) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = end
                        }
                        (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotation })
        )
        Core.getPosition
        |. Tokens.portToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely (Core.symbol ":")
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser State (Node Documentation -> Parser State (Node Declaration.Declaration))
typeOrTypeAliasDefinitionAfterDocumentation =
    Core.map
        (\() ->
            \with ->
                \((Node documentationRange _) as documentation) ->
                    Combine.succeed (with documentationRange.start (Just documentation))
        )
        (Core.symbol "type")
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep typeOrTypeAliasDefinitionWithAfterTypePrefix


typeOrTypeAliasDefinitionWithoutDocumentation : Parser State (Node Declaration.Declaration)
typeOrTypeAliasDefinitionWithoutDocumentation =
    Core.map
        (\( row, column ) ->
            \with ->
                with { row = row, column = column } Nothing
        )
        Core.getPosition
        |. Core.symbol "type"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep typeOrTypeAliasDefinitionWithAfterTypePrefix


typeOrTypeAliasDefinitionWithAfterTypePrefix : Parser State (Location -> Maybe (Node String) -> Node Declaration.Declaration)
typeOrTypeAliasDefinitionWithAfterTypePrefix =
    Combine.oneOf
        [ typeAliasDefinitionAfterTypePrefix
        , customTypeDefinitionAfterTypePrefix
        ]


typeAliasDefinitionAfterTypePrefix : Parser State (Location -> Maybe (Node Documentation) -> Node Declaration)
typeAliasDefinitionAfterTypePrefix =
    Core.map
        (\() ->
            \name ->
                \generics ->
                    \((Node { end } _) as typeAnnotation) ->
                        \start documentation ->
                            Node { start = start, end = end }
                                (Declaration.AliasDeclaration
                                    { documentation = documentation
                                    , name = name
                                    , generics = generics
                                    , typeAnnotation = typeAnnotation
                                    }
                                )
        )
        Tokens.aliasToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keepFromCore (Node.parserCore Tokens.typeName)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


customTypeDefinitionAfterTypePrefix : Parser State (Location -> Maybe (Node Documentation) -> Node Declaration)
customTypeDefinitionAfterTypePrefix =
    Core.map
        (\name ->
            \generics ->
                \((Node headVariantRange _) as headVariant) ->
                    \tailVariantsReverse ->
                        \start documentation ->
                            let
                                end : Location
                                end =
                                    case tailVariantsReverse of
                                        (Node range _) :: _ ->
                                            range.end

                                        [] ->
                                            headVariantRange.end
                            in
                            Node { start = start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = documentation
                                    , name = name
                                    , generics = generics
                                    , constructors = headVariant :: List.reverse tailVariantsReverse
                                    }
                                )
        )
        (Node.parserCore Tokens.typeName)
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep valueConstructor
        |> Combine.keep
            (Combine.manyWithoutReverse
                (Combine.maybeIgnore Layout.layout
                    |> Combine.backtrackable
                    |> Combine.ignoreEntirely Tokens.pipe
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith valueConstructor
                )
            )


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    (Tokens.typeName
        |> Node.parserCoreMap
            (\((Node variantNameRange _) as variantNameNode) ->
                \argumentsReverse ->
                    let
                        fullEnd : Location
                        fullEnd =
                            case argumentsReverse of
                                (Node lastArgRange _) :: _ ->
                                    lastArgRange.end

                                [] ->
                                    variantNameRange.end
                    in
                    Node
                        { start = variantNameRange.start, end = fullEnd }
                        { name = variantNameNode, arguments = List.reverse argumentsReverse }
            )
    )
        |> Combine.fromCoreKeep
            (Combine.manyWithoutReverse
                (Combine.maybeIgnore Layout.layout
                    |> Combine.backtrackable
                    |> Combine.continueWith typeAnnotationNoFnExcludingTypedWithArguments
                )
            )


typeGenericList : Parser State (List (Node String))
typeGenericList =
    Combine.many
        (Node.parserCore Tokens.functionName
            |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        )
