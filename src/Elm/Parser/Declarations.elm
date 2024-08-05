module Elm.Parser.Declarations exposing (declaration)

import CustomParser exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Expression exposing (expression)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import ParserWithComments exposing (Comments, WithComments)
import Rope


declaration : Parser (WithComments (Node Declaration))
declaration =
    CustomParser.oneOf
        [ functionDeclarationWithoutDocumentation
        , declarationWithDocumentation
        , typeOrTypeAliasDefinitionWithoutDocumentation
        , portDeclarationWithoutDocumentation
        , infixDeclaration
        ]


declarationWithDocumentation : Parser (WithComments (Node Declaration))
declarationWithDocumentation =
    CustomParser.map3
        (\documentation commentsAfterDocumentation afterDocumentation ->
            let
                start : Location
                start =
                    (Node.range documentation).start
            in
            case afterDocumentation.syntax of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    let
                        (Node startNameRange startName) =
                            functionDeclarationAfterDocumentation.startName
                    in
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (Node implementationNameRange implementationName) =
                                    signature.implementationName
                            in
                            if implementationName == startName then
                                let
                                    (Node expressionRange _) =
                                        functionDeclarationAfterDocumentation.expression
                                in
                                { comments =
                                    commentsAfterDocumentation
                                        |> Rope.prependTo afterDocumentation.comments
                                , syntax =
                                    Node { start = start, end = expressionRange.end }
                                        (Declaration.FunctionDeclaration
                                            { documentation = Just documentation
                                            , signature =
                                                Just
                                                    (Node.combine Signature
                                                        functionDeclarationAfterDocumentation.startName
                                                        signature.typeAnnotation
                                                    )
                                            , declaration =
                                                Node { start = implementationNameRange.start, end = expressionRange.end }
                                                    { name = signature.implementationName
                                                    , arguments = functionDeclarationAfterDocumentation.arguments
                                                    , expression = functionDeclarationAfterDocumentation.expression
                                                    }
                                            }
                                        )
                                }
                                    |> CustomParser.succeed

                            else
                                CustomParser.problem
                                    ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)

                        Nothing ->
                            let
                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments =
                                commentsAfterDocumentation
                                    |> Rope.prependTo afterDocumentation.comments
                            , syntax =
                                Node { start = start, end = expressionRange.end }
                                    (Declaration.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = startNameRange.start, end = expressionRange.end }
                                                { name = functionDeclarationAfterDocumentation.startName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                            }
                                |> CustomParser.succeed

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
                    { comments =
                        commentsAfterDocumentation
                            |> Rope.prependTo afterDocumentation.comments
                    , syntax =
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
                    }
                        |> CustomParser.succeed

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments =
                        commentsAfterDocumentation
                            |> Rope.prependTo afterDocumentation.comments
                    , syntax =
                        Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Just documentation
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }
                        |> CustomParser.succeed

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    { comments =
                        Rope.one documentation
                            |> Rope.filledPrependTo
                                commentsAfterDocumentation
                            |> Rope.prependTo afterDocumentation.comments
                    , syntax =
                        Node
                            { start = portDeclarationAfterName.startLocation
                            , end = typeAnnotationRange.end
                            }
                            (Declaration.PortDeclaration
                                { name = portDeclarationAfterName.name
                                , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                }
                            )
                    }
                        |> CustomParser.succeed
        )
        Comments.declarationDocumentation
        Layout.layoutStrict
        (CustomParser.oneOf
            [ functionAfterDocumentation
            , typeOrTypeAliasDefinitionAfterDocumentation
            , portDeclarationAfterDocumentation
            ]
        )
        |> CustomParser.andThen identity


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : Node String
        , signature :
            Maybe
                { typeAnnotation : Node TypeAnnotation
                , implementationName : Node String
                }
        , arguments : List (Node Pattern)
        , expression : Node Expression
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


functionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    CustomParser.map7
        (\startNameStart startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            { comments =
                commentsAfterStartName
                    |> Rope.prependTo
                        (case maybeSignature of
                            Nothing ->
                                Rope.empty

                            Just signature ->
                                signature.comments
                        )
                    |> Rope.prependTo arguments.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo result.comments
            , syntax =
                FunctionDeclarationAfterDocumentation
                    { startName =
                        Node.singleLineStringFrom startNameStart
                            startName
                    , signature = maybeSignature |> Maybe.map .syntax
                    , arguments = arguments.syntax
                    , expression = result.syntax
                    }
            }
        )
        CustomParser.getPosition
        -- infix declarations itself don't have documentation
        Tokens.functionName
        Layout.maybeLayout
        (CustomParser.oneOf
            [ CustomParser.map7
                (\() commentsBeforeTypeAnnotation typeAnnotationResult commentsAfterTypeAnnotation implementationNameStart implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> Rope.prependTo typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfterTypeAnnotation
                                |> Rope.prependTo afterImplementationName
                        , syntax =
                            { implementationName =
                                Node.singleLineStringFrom implementationNameStart
                                    implementationName
                            , typeAnnotation = typeAnnotationResult.syntax
                            }
                        }
                )
                Tokens.colon
                Layout.maybeLayout
                TypeAnnotation.typeAnnotation
                Layout.layoutStrict
                CustomParser.getPosition
                Tokens.functionName
                Layout.maybeLayout
            , CustomParser.succeed Nothing
            ]
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    CustomParser.map7
        (\startNameStart startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            let
                allComments : Comments
                allComments =
                    commentsAfterStartName
                        |> Rope.prependTo
                            (case maybeSignature of
                                Nothing ->
                                    Rope.empty

                                Just signature ->
                                    signature.comments
                            )
                        |> Rope.prependTo arguments.comments
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo result.comments

                startNameNode : Node String
                startNameNode =
                    Node.singleLineStringFrom startNameStart
                        startName
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            result.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = arguments.syntax
                                        , expression = result.syntax
                                        }
                                }
                            )
                    }
                        |> CustomParser.succeed

                Just signature ->
                    let
                        (Node implementationNameRange implementationName) =
                            signature.implementationName
                    in
                    if implementationName == startName then
                        let
                            (Node expressionRange _) =
                                result.syntax
                        in
                        { comments = allComments
                        , syntax =
                            Node { start = startNameStart, end = expressionRange.end }
                                (Declaration.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                    , declaration =
                                        Node { start = implementationNameRange.start, end = expressionRange.end }
                                            { name = signature.implementationName
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
                                )
                        }
                            |> CustomParser.succeed

                    else
                        CustomParser.problem
                            ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        CustomParser.getPosition
        Tokens.functionNameNotInfix
        Layout.maybeLayout
        (CustomParser.oneOf
            [ CustomParser.map7
                (\() commentsBeforeTypeAnnotation typeAnnotationResult commentsAfterTypeAnnotation implementationNameStart implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> Rope.prependTo typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfterTypeAnnotation
                                |> Rope.prependTo afterImplementationName
                        , implementationName =
                            Node.singleLineStringFrom implementationNameStart
                                implementationName
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                Tokens.colon
                Layout.maybeLayout
                TypeAnnotation.typeAnnotation
                Layout.layoutStrict
                CustomParser.getPosition
                Tokens.functionName
                Layout.maybeLayout
            , CustomParser.succeed Nothing
            ]
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression
        |> CustomParser.andThen identity


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (CustomParser.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Patterns.pattern
            Layout.maybeLayout
        )


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    CustomParser.map11
        (\() startRow commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual ((Node fnRange _) as fn) ->
            { comments =
                commentsAfterInfix
                    |> Rope.prependTo commentsAfterDirection
                    |> Rope.prependTo commentsAfterPrecedence
                    |> Rope.prependTo commentsAfterOperator
                    |> Rope.prependTo commentsAfterEqual
            , syntax =
                Node
                    { start = { row = startRow, column = 1 }
                    , end = fnRange.end
                    }
                    (Declaration.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
            }
        )
        (CustomParser.keyword "infix" ())
        CustomParser.getRow
        Layout.maybeLayout
        (Node.parserCore infixDirection)
        Layout.maybeLayout
        (Node.parserCore CustomParser.int)
        Layout.maybeLayout
        (Node.parserCore
            (CustomParser.map3
                (\() prefixOperator () -> prefixOperator)
                Tokens.parensStart
                Tokens.prefixOperatorToken
                Tokens.parensEnd
            )
        )
        (Layout.maybeLayoutUntilIgnored CustomParser.symbol "=")
        Layout.maybeLayout
        (Node.parserCore Tokens.functionName)


infixDirection : CustomParser.Parser Infix.InfixDirection
infixDirection =
    CustomParser.oneOf
        [ CustomParser.keyword "right" Infix.Right
        , CustomParser.keyword "left" Infix.Left
        , CustomParser.keyword "non" Infix.Non
        ]


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    CustomParser.map8
        (\() startRow commentsAfterPort nameStart name commentsAfterName commentsAfterColon typeAnnotationResult ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo typeAnnotationResult.comments
                    |> Rope.prependTo commentsAfterColon
            , syntax =
                PortDeclarationAfterDocumentation
                    { startLocation = { row = startRow, column = 1 }
                    , name =
                        Node.singleLineStringFrom nameStart
                            name
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        Tokens.portToken
        CustomParser.getRow
        Layout.maybeLayout
        CustomParser.getPosition
        Tokens.functionName
        (Layout.maybeLayoutUntilIgnored CustomParser.symbol ":")
        Layout.maybeLayout
        typeAnnotation


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    CustomParser.map8
        (\() startRow commentsAfterPort nameStart name commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (Node { end } _) =
                    typeAnnotationResult.syntax
            in
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                Node
                    { start = { row = startRow, column = 1 }
                    , end = end
                    }
                    (Declaration.PortDeclaration
                        { name =
                            Node.singleLineStringFrom nameStart
                                name
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    )
            }
        )
        Tokens.portToken
        CustomParser.getRow
        Layout.maybeLayout
        CustomParser.getPosition
        Tokens.functionName
        (Layout.maybeLayoutUntilIgnored CustomParser.symbol ":")
        Layout.maybeLayout
        typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    CustomParser.map3
        (\() commentsAfterType declarationAfterDocumentation ->
            { comments = commentsAfterType |> Rope.prependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
        )
        (CustomParser.keyword "type" ())
        Layout.maybeLayout
        (CustomParser.oneOf
            [ typeAliasDefinitionAfterDocumentationAfterTypePrefix
            , customTypeDefinitionAfterDocumentationAfterTypePrefix
            ]
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    CustomParser.map8
        (\() commentsAfterAlias nameStart name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationAfterDocumentation
                    { name =
                        Node.singleLineStringFrom nameStart
                            name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        Tokens.aliasToken
        Layout.maybeLayout
        CustomParser.getPosition
        Tokens.typeName
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    CustomParser.map7
        (\nameStart name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationAfterDocumentation
                    { name =
                        Node.singleLineStringFrom nameStart
                            name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        CustomParser.getPosition
        Tokens.typeName
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructor
        (ParserWithComments.manyWithoutReverse
            (CustomParser.map3
                (\commentsBeforePipe commentsAfterPipe variantResult ->
                    { comments =
                        commentsBeforePipe
                            |> Rope.prependTo commentsAfterPipe
                            |> Rope.prependTo variantResult.comments
                    , syntax = variantResult.syntax
                    }
                )
                (Layout.maybeLayoutUntilIgnored CustomParser.symbol "|" |> CustomParser.backtrackable)
                Layout.maybeLayout
                valueConstructor
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    CustomParser.map4
        (\() startRow commentsAfterType afterStart ->
            let
                start : Location
                start =
                    { row = startRow, column = 1 }
            in
            { comments = commentsAfterType |> Rope.prependTo afterStart.comments
            , syntax =
                case afterStart.syntax of
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
            }
        )
        (CustomParser.keyword "type" ())
        CustomParser.getRow
        Layout.maybeLayout
        (CustomParser.oneOf
            [ typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            , customTypeDefinitionWithoutDocumentationAfterTypePrefix
            ]
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    CustomParser.map8
        (\() commentsAfterAlias nameStart name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { name =
                        Node.singleLineStringFrom nameStart
                            name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        Tokens.aliasToken
        Layout.maybeLayout
        CustomParser.getPosition
        Tokens.typeName
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    CustomParser.map7
        (\nameStart name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationWithoutDocumentation
                    { name =
                        Node.singleLineStringFrom nameStart
                            name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        CustomParser.getPosition
        Tokens.typeName
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructor
        (ParserWithComments.manyWithoutReverse
            (CustomParser.map3
                (\commentsBeforePipe commentsAfterPipe variantResult ->
                    { comments =
                        commentsBeforePipe
                            |> Rope.prependTo commentsAfterPipe
                            |> Rope.prependTo variantResult.comments
                    , syntax = variantResult.syntax
                    }
                )
                (Layout.maybeLayoutUntilIgnored CustomParser.symbol "|" |> CustomParser.backtrackable)
                Layout.maybeLayout
                valueConstructor
            )
        )


valueConstructor : Parser (WithComments (Node ValueConstructor))
valueConstructor =
    CustomParser.map3
        (\nameStart name argumentsReverse ->
            let
                nameRange : Range
                nameRange =
                    Node.singleLineStringRangeFrom nameStart name

                fullEnd : Location
                fullEnd =
                    case argumentsReverse.syntax of
                        (Node lastArgRange _) :: _ ->
                            lastArgRange.end

                        [] ->
                            nameRange.end
            in
            { comments = argumentsReverse.comments
            , syntax =
                Node { start = nameStart, end = fullEnd }
                    { name = Node nameRange name
                    , arguments = List.reverse argumentsReverse.syntax
                    }
            }
        )
        CustomParser.getPosition
        Tokens.typeName
        (ParserWithComments.manyWithoutReverse
            (CustomParser.map2
                (\commentsBefore typeAnnotationResult ->
                    { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                typeAnnotationNoFnExcludingTypedWithArguments
            )
        )


typeGenericListEquals : Parser (WithComments (List (Node String)))
typeGenericListEquals =
    ParserWithComments.until Tokens.equal
        (CustomParser.map3
            (\nameStart name commentsAfterName ->
                { comments = commentsAfterName
                , syntax =
                    Node.singleLineStringFrom nameStart
                        name
                }
            )
            CustomParser.getPosition
            Tokens.functionName
            Layout.maybeLayout
        )
