module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|.), (|=))


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ Core.map
            (\documentation ->
                \afterDocumentation ->
                    let
                        start : Location
                        start =
                            (Node.range documentation).start
                    in
                    case afterDocumentation of
                        FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                            let
                                (Node startNameRange startName) =
                                    functionDeclarationAfterDocumentation.startName
                            in
                            case functionDeclarationAfterDocumentation.afterStartName of
                                FunctionDeclarationWithSignatureAfterName withSignature ->
                                    let
                                        (Node implementationNameRange implementationName) =
                                            withSignature.implementationName
                                    in
                                    if implementationName == startName then
                                        let
                                            (Node expressionRange _) =
                                                withSignature.expression
                                        in
                                        Combine.succeed
                                            (Node { start = start, end = expressionRange.end }
                                                (Declaration.FunctionDeclaration
                                                    { documentation = Just documentation
                                                    , signature =
                                                        Just
                                                            (Node.combine Signature
                                                                functionDeclarationAfterDocumentation.startName
                                                                withSignature.typeAnnotation
                                                            )
                                                    , declaration =
                                                        Node { start = implementationNameRange.start, end = expressionRange.end }
                                                            { name = withSignature.implementationName
                                                            , arguments = withSignature.arguments
                                                            , expression = withSignature.expression
                                                            }
                                                    }
                                                )
                                            )

                                    else
                                        Combine.problem
                                            ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)

                                FunctionDeclarationWithoutSignatureAfterName withoutSignature ->
                                    let
                                        (Node expressionRange _) =
                                            withoutSignature.expression
                                    in
                                    Node { start = start, end = expressionRange.end }
                                        (Declaration.FunctionDeclaration
                                            { documentation = Just documentation
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = startNameRange.start, end = expressionRange.end }
                                                    { name = functionDeclarationAfterDocumentation.startName
                                                    , arguments = withoutSignature.arguments
                                                    , expression = withoutSignature.expression
                                                    }
                                            }
                                        )
                                        |> Combine.succeed

                        TypeDeclarationAfterDocumentation typeDeclarationAfterDocumentation ->
                            let
                                end : Location
                                end =
                                    case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                        (Node range _) :: _ ->
                                            range.end

                                        [] ->
                                            let
                                                (Node headVariantRange _) =
                                                    typeDeclarationAfterDocumentation.headVariant
                                            in
                                            headVariantRange.end
                            in
                            Node { start = start, end = end }
                                (Declaration.CustomTypeDeclaration
                                    { documentation = Just documentation
                                    , name = typeDeclarationAfterDocumentation.name
                                    , generics = typeDeclarationAfterDocumentation.parameters
                                    , constructors =
                                        typeDeclarationAfterDocumentation.headVariant
                                            :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                    }
                                )
                                |> Combine.succeed

                        TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                            let
                                (Node typeAnnotationRange _) =
                                    typeAliasDeclarationAfterDocumentation.typeAnnotation
                            in
                            Node { start = start, end = typeAnnotationRange.end }
                                (Declaration.AliasDeclaration
                                    { documentation = Just documentation
                                    , name = typeAliasDeclarationAfterDocumentation.name
                                    , generics = typeAliasDeclarationAfterDocumentation.parameters
                                    , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                    }
                                )
                                |> Combine.succeed

                        PortDeclarationAfterDocumentation portDeclarationAfterName ->
                            let
                                (Node typeAnnotationRange _) =
                                    portDeclarationAfterName.typeAnnotation
                            in
                            Combine.succeed
                                (Node
                                    { start = portDeclarationAfterName.startLocation
                                    , end = typeAnnotationRange.end
                                    }
                                    (Declaration.PortDeclaration
                                        { name = portDeclarationAfterName.name
                                        , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                        }
                                    )
                                )
                                |> Combine.ignore (Combine.modifyState (State.addCommentAccordingToRange documentation))
            )
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


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : Node String
        , afterStartName : FunctionDeclarationAfterName
        }
    | TypeDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }
    | PortDeclarationAfterDocumentation
        { startLocation : Location
        , name : Node String
        , typeAnnotation : Node TypeAnnotation
        }


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = TypeDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }


functionAfterDocumentation : Parser State DeclarationAfterDocumentation
functionAfterDocumentation =
    Node.parserCoreMap
        (\startNameNode ->
            \afterStartName ->
                FunctionDeclarationAfterDocumentation
                    { startName = startNameNode, afterStartName = afterStartName }
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep functionDeclarationAfterName


functionDeclarationAfterName : Parser State FunctionDeclarationAfterName
functionDeclarationAfterName =
    Combine.oneOf
        [ letFunctionWithSignatureAfterName
        , letFunctionWithoutSignatureAfterName
        ]


letFunctionWithoutSignatureAfterName : Parser State FunctionDeclarationAfterName
letFunctionWithoutSignatureAfterName =
    Combine.map
        (\args ->
            \result ->
                FunctionDeclarationWithoutSignatureAfterName
                    { arguments = args
                    , expression = result
                    }
        )
        (Combine.many (Patterns.pattern |> Combine.ignore Layout.maybeLayout))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep expression


letFunctionWithSignatureAfterName : Parser State FunctionDeclarationAfterName
letFunctionWithSignatureAfterName =
    Tokens.colon
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.continueWith
            (Combine.map
                (\typeAnnotation ->
                    \implementationNameNode ->
                        \arguments ->
                            \result ->
                                FunctionDeclarationWithSignatureAfterName
                                    { typeAnnotation = typeAnnotation
                                    , implementationName = implementationNameNode
                                    , arguments = arguments
                                    , expression = result
                                    }
                )
                TypeAnnotation.typeAnnotation
            )
        |> Combine.ignore Layout.layoutStrict
        |> Combine.keepFromCore (Tokens.functionName |> Node.parserCore)
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore Layout.maybeLayout))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep expression


functionDeclarationWithoutDocumentation : Parser State (Node Declaration)
functionDeclarationWithoutDocumentation =
    Node.parserCoreMap
        (\((Node startNameRange startName) as startNameNode) ->
            \afterStartStartName ->
                let
                    start : Location
                    start =
                        startNameRange.start
                in
                case afterStartStartName of
                    FunctionDeclarationWithSignatureAfterName withSignature ->
                        let
                            (Node implementationNameRange implementationName) =
                                withSignature.implementationName
                        in
                        if implementationName == startName then
                            let
                                (Node expressionRange _) =
                                    withSignature.expression
                            in
                            Core.succeed
                                (Node { start = start, end = expressionRange.end }
                                    (Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Just (Node.combine Signature startNameNode withSignature.typeAnnotation)
                                        , declaration =
                                            Node { start = implementationNameRange.start, end = expressionRange.end }
                                                { name = withSignature.implementationName
                                                , arguments = withSignature.arguments
                                                , expression = withSignature.expression
                                                }
                                        }
                                    )
                                )

                        else
                            Core.problem
                                ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)

                    FunctionDeclarationWithoutSignatureAfterName withoutSignature ->
                        let
                            (Node expressionRange _) =
                                withoutSignature.expression
                        in
                        Node { start = start, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameRange.start, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = withoutSignature.arguments
                                        , expression = withoutSignature.expression
                                        }
                                }
                            )
                            |> Core.succeed
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep functionDeclarationAfterName
        |> Combine.flattenFromCore


type FunctionDeclarationAfterName
    = FunctionDeclarationWithSignatureAfterName
        { typeAnnotation : Node TypeAnnotation
        , implementationName : Node String
        , arguments : List (Node Pattern)
        , expression : Node Expression
        }
    | FunctionDeclarationWithoutSignatureAfterName
        { arguments : List (Node Pattern)
        , expression : Node Expression
        }


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


portDeclarationAfterDocumentation : Parser State DeclarationAfterDocumentation
portDeclarationAfterDocumentation =
    Core.map
        (\( startRow, startColumn ) ->
            \name ->
                \typeAnnotation ->
                    PortDeclarationAfterDocumentation
                        { startLocation = { row = startRow, column = startColumn }
                        , name = name
                        , typeAnnotation = typeAnnotation
                        }
        )
        Core.getPosition
        |. Tokens.portToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep
            (Node.parserCore Tokens.functionName
                |> Combine.fromCoreIgnore Layout.maybeLayout
                |> Combine.ignoreEntirely (Core.symbol ":")
                |> Combine.ignore Layout.maybeLayout
            )
        |> Combine.keep typeAnnotation


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
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignoreEntirely (Core.symbol ":")
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser State DeclarationAfterDocumentation
typeOrTypeAliasDefinitionAfterDocumentation =
    Core.symbol "type"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ typeAliasDefinitionAfterDocumentationAfterTypePrefix
                , customTypeDefinitionAfterDocumentationAfterTypePrefix
                ]
            )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser State DeclarationAfterDocumentation
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    Tokens.aliasToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.continueWithCore
            (Node.parserCoreMap
                (\name ->
                    \parameters ->
                        \typeAnnotation ->
                            TypeAliasDeclarationAfterDocumentation
                                { name = name
                                , parameters = parameters
                                , typeAnnotation = typeAnnotation
                                }
                )
                Tokens.typeName
            )
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser State DeclarationAfterDocumentation
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    Core.map
        (\name ->
            \parameters ->
                \headVariant ->
                    \tailVariantsReverse ->
                        TypeDeclarationAfterDocumentation
                            { name = name
                            , parameters = parameters
                            , headVariant = headVariant
                            , tailVariantsReverse = tailVariantsReverse
                            }
        )
        (Node.parserCore Tokens.typeName)
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep valueConstructor
        |> Combine.keep
            (Combine.manyWithoutReverse
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.ignoreEntirely Tokens.pipe
                    |> Combine.ignore Layout.maybeLayout
                    |> Combine.continueWith valueConstructor
                )
            )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser State (Node Declaration.Declaration)
typeOrTypeAliasDefinitionWithoutDocumentation =
    Core.map
        (\( startRow, startColumn ) ->
            let
                start : Location
                start =
                    { row = startRow, column = startColumn }
            in
            \afterStart ->
                case afterStart of
                    TypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                        let
                            end : Location
                            end =
                                case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                    (Node range _) :: _ ->
                                        range.end

                                    [] ->
                                        let
                                            (Node headVariantRange _) =
                                                typeDeclarationAfterDocumentation.headVariant
                                        in
                                        headVariantRange.end
                        in
                        Node { start = start, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )

                    TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                        let
                            (Node typeAnnotationRange _) =
                                typeAliasDeclarationAfterDocumentation.typeAnnotation
                        in
                        Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
        )
        Core.getPosition
        |. Core.symbol "type"
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep
            (Combine.oneOf
                [ typeAliasDefinitionWithoutDocumentationAfterTypePrefix
                , customTypeDefinitionWithoutDocumentationAfterTypePrefix
                ]
            )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser State TypeOrTypeAliasDeclarationWithoutDocumentation
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    Tokens.aliasToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.continueWithCore
            (Node.parserCoreMap
                (\name ->
                    \parameters ->
                        \typeAnnotation ->
                            TypeAliasDeclarationWithoutDocumentation
                                { name = name
                                , parameters = parameters
                                , typeAnnotation = typeAnnotation
                                }
                )
                Tokens.typeName
            )
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser State TypeOrTypeAliasDeclarationWithoutDocumentation
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    Core.map
        (\name ->
            \parameters ->
                \headVariant ->
                    \tailVariantsReverse ->
                        TypeDeclarationWithoutDocumentation
                            { name = name
                            , parameters = parameters
                            , headVariant = headVariant
                            , tailVariantsReverse = tailVariantsReverse
                            }
        )
        (Node.parserCore Tokens.typeName)
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep typeGenericList
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep valueConstructor
        |> Combine.keep
            (Combine.manyWithoutReverse
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.ignoreEntirely Tokens.pipe
                    |> Combine.ignore Layout.maybeLayout
                    |> Combine.continueWith valueConstructor
                )
            )


valueConstructor : Parser State (Node ValueConstructor)
valueConstructor =
    Tokens.typeName
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
        |> Combine.fromCoreKeep
            (Combine.manyWithoutReverse
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.continueWith typeAnnotationNoFnExcludingTypedWithArguments
                )
            )


typeGenericList : Parser State (List (Node String))
typeGenericList =
    Combine.many
        (Node.parserCore Tokens.functionName
            |> Combine.fromCoreIgnore Layout.maybeLayout
        )
