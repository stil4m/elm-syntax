module Elm.Parser.Declarations exposing (declaration)

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
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


declaration : Parser (WithComments (Node Declaration))
declaration =
    Core.oneOf
        [ (Core.map
            (\documentation ->
                \commentsAfterDocumentation ->
                    \afterDocumentation ->
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
                                                Rope.flatFromList
                                                    [ commentsAfterDocumentation, afterDocumentation.comments ]
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
                                                |> Core.succeed

                                        else
                                            Core.problem
                                                ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)

                                    Nothing ->
                                        let
                                            (Node expressionRange _) =
                                                functionDeclarationAfterDocumentation.expression
                                        in
                                        { comments =
                                            Rope.flatFromList
                                                [ commentsAfterDocumentation, afterDocumentation.comments ]
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
                                            |> Core.succeed

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
                                    Rope.flatFromList
                                        [ commentsAfterDocumentation, afterDocumentation.comments ]
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
                                    |> Core.succeed

                            TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                                let
                                    (Node typeAnnotationRange _) =
                                        typeAliasDeclarationAfterDocumentation.typeAnnotation
                                in
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterDocumentation, afterDocumentation.comments ]
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
                                    |> Core.succeed

                            PortDeclarationAfterDocumentation portDeclarationAfterName ->
                                let
                                    (Node typeAnnotationRange _) =
                                        portDeclarationAfterName.typeAnnotation
                                in
                                { comments =
                                    Rope.flatFromList
                                        [ Rope.one documentation
                                        , commentsAfterDocumentation
                                        , afterDocumentation.comments
                                        ]
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
                                    |> Core.succeed
            )
            Comments.declarationDocumentation
            |= Layout.layoutStrict
            |= Core.oneOf
                [ functionAfterDocumentation
                , typeOrTypeAliasDefinitionAfterDocumentation
                , portDeclarationAfterDocumentation
                ]
          )
            |> Core.andThen identity
        , infixDeclaration
        , functionDeclarationWithoutDocumentation
        , typeOrTypeAliasDefinitionWithoutDocumentation
        , portDeclarationWithoutDocumentation
        ]


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
    (Node.parserCoreMap
        (\startName ->
            \signature ->
                \arguments ->
                    \result ->
                        FunctionDeclarationAfterDocumentation
                            { startName = startName
                            , signature = signature
                            , arguments = arguments
                            , expression = result
                            }
        )
        Tokens.functionName
        |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
        |> ParserWithComments.keep
            (ParserWithComments.maybe
                ((Tokens.colon
                    |> Parser.Extra.continueWith
                        (Core.map
                            (\commentsBeforeTypeAnnotation ->
                                \typeAnnotationResult ->
                                    \commentsAfterTypeAnnotation ->
                                        \implementationNameNode ->
                                            \afterImplementationName ->
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsBeforeTypeAnnotation
                                                        , typeAnnotationResult.comments
                                                        , commentsAfterTypeAnnotation
                                                        , afterImplementationName
                                                        ]
                                                , syntax =
                                                    { implementationName = implementationNameNode
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                                                }
                            )
                            Layout.maybeLayout
                        )
                 )
                    |= TypeAnnotation.typeAnnotation
                    |= Layout.layoutStrict
                    |= (Tokens.functionName |> Node.parserCore)
                    |= Layout.maybeLayout
                )
            )
        |> ParserWithComments.keep
            (ParserWithComments.many
                (Patterns.pattern |> ParserWithComments.ignore Layout.maybeLayout)
            )
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    (Node.parserCoreMap
        (\((Node { start } startName) as startNameNode) ->
            \maybeSignature ->
                \arguments ->
                    \result ->
                        case maybeSignature of
                            Nothing ->
                                let
                                    (Node expressionRange _) =
                                        result
                                in
                                Node { start = start, end = expressionRange.end }
                                    (Declaration.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = start, end = expressionRange.end }
                                                { name = startNameNode
                                                , arguments = arguments
                                                , expression = result
                                                }
                                        }
                                    )
                                    |> Core.succeed

                            Just signature ->
                                let
                                    (Node implementationNameRange implementationName) =
                                        signature.implementationName
                                in
                                if implementationName == startName then
                                    let
                                        (Node expressionRange _) =
                                            result
                                    in
                                    Core.succeed
                                        (Node { start = start, end = expressionRange.end }
                                            (Declaration.FunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                                , declaration =
                                                    Node { start = implementationNameRange.start, end = expressionRange.end }
                                                        { name = signature.implementationName
                                                        , arguments = arguments
                                                        , expression = result
                                                        }
                                                }
                                            )
                                        )

                                else
                                    Core.problem
                                        ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        Tokens.functionName
        |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
        |> ParserWithComments.keep
            (ParserWithComments.maybe
                ((Tokens.colon
                    |> Parser.Extra.continueWith
                        (Core.map
                            (\commentsBeforeTypeAnnotation ->
                                \typeAnnotationResult ->
                                    \commentsAfterTypeAnnotation ->
                                        \implementationNameNode ->
                                            \afterImplementationName ->
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsBeforeTypeAnnotation
                                                        , typeAnnotationResult.comments
                                                        , commentsAfterTypeAnnotation
                                                        , afterImplementationName
                                                        ]
                                                , syntax =
                                                    { implementationName = implementationNameNode
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                                                }
                            )
                            Layout.maybeLayout
                        )
                 )
                    |= TypeAnnotation.typeAnnotation
                    |= Layout.layoutStrict
                    |= (Tokens.functionName |> Node.parserCore)
                    |= Layout.maybeLayout
                )
            )
        |> ParserWithComments.keep
            (ParserWithComments.many
                (Patterns.pattern |> ParserWithComments.ignore Layout.maybeLayout)
            )
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep expression
        |> ParserWithComments.flattenFromCore


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    (Core.map
        (\startRow ->
            \direction ->
                \precedence ->
                    \operator ->
                        \((Node fnRange _) as fn) ->
                            Node
                                { start = { row = startRow, column = 1 }
                                , end = fnRange.end
                                }
                                (Declaration.InfixDeclaration
                                    { direction = direction, precedence = precedence, operator = operator, function = fn }
                                )
        )
        Core.getRow
        |. Core.keyword "infix"
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.keepFromCore (Node.parserCore infixDirection)
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keepFromCore (Node.parserCore Core.int)
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keepFromCore operatorWithParens
        |> ParserWithComments.ignore Layout.layout
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keepFromCore (Node.parserCore Tokens.functionName)


operatorWithParens : Core.Parser (Node String)
operatorWithParens =
    (Tokens.parensStart
        |> Parser.Extra.continueWith Tokens.prefixOperatorToken
    )
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


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    Core.map
        (\startRow ->
            \name ->
                \typeAnnotation ->
                    PortDeclarationAfterDocumentation
                        { startLocation = { row = startRow, column = 1 }
                        , name = name
                        , typeAnnotation = typeAnnotation
                        }
        )
        Core.getRow
        |. Tokens.portToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.keep
            ((Node.parserCore Tokens.functionName
                |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
             )
                |. Core.symbol ":"
                |> ParserWithComments.ignore Layout.maybeLayout
            )
        |> ParserWithComments.keep typeAnnotation


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    (Core.map
        (\startRow ->
            \name ->
                \((Node { end } _) as typeAnnotation) ->
                    Node
                        { start = { row = startRow, column = 1 }
                        , end = end
                        }
                        (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotation })
        )
        Core.getRow
        |. Tokens.portToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.keepFromCore (Node.parserCore Tokens.functionName)
        |> ParserWithComments.ignore Layout.maybeLayout
    )
        |. Core.symbol ":"
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    (Core.symbol "type"
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsAfterType ->
                    \declarationAfterDocumentation ->
                        { comments = Rope.flatFromList [ commentsAfterType, declarationAfterDocumentation.comments ]
                        , syntax = declarationAfterDocumentation.syntax
                        }
                )
                Layout.layout
            )
    )
        |= Core.oneOf
            [ typeAliasDefinitionAfterDocumentationAfterTypePrefix
            , customTypeDefinitionAfterDocumentationAfterTypePrefix
            ]


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    (Tokens.aliasToken
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsAfterAlias ->
                    \name ->
                        \commentsAfterName ->
                            \parameters ->
                                \commentsAfterEquals ->
                                    \typeAnnotationResult ->
                                        { comments =
                                            Rope.flatFromList
                                                [ commentsAfterAlias
                                                , commentsAfterName
                                                , parameters.comments
                                                , commentsAfterEquals
                                                , typeAnnotationResult.comments
                                                ]
                                        , syntax =
                                            TypeAliasDeclarationAfterDocumentation
                                                { name = name
                                                , parameters = parameters.syntax
                                                , typeAnnotation = typeAnnotationResult.syntax
                                                }
                                        }
                )
                Layout.layout
            )
    )
        |= Node.parserCore Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericList
        |. Tokens.equal
        |= Layout.maybeLayout
        |= typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    (Node.parserCoreMap
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
        Tokens.typeName
        |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
        |> ParserWithComments.keep typeGenericList
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep valueConstructor
        |> ParserWithComments.keep
            (ParserWithComments.manyWithoutReverse
                (Core.map
                    (\commentsBeforePipe ->
                        \commentsAfterPipe ->
                            \variantResult ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsBeforePipe
                                        , commentsAfterPipe
                                        , variantResult.comments
                                        ]
                                , syntax = variantResult.syntax
                                }
                    )
                    (Layout.maybeLayout |> Core.backtrackable)
                    |. Tokens.pipe
                    |= Layout.maybeLayout
                    |= valueConstructor
                )
            )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    Core.map
        (\startRow ->
            let
                start : Location
                start =
                    { row = startRow, column = 1 }
            in
            \commentsAfterType ->
                \afterStart ->
                    { comments = Rope.flatFromList [ commentsAfterType, afterStart.comments ]
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
        Core.getRow
        |. Core.symbol "type"
        |= Layout.layout
        |= Core.oneOf
            [ typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            , customTypeDefinitionWithoutDocumentationAfterTypePrefix
            ]


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    (Tokens.aliasToken
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsAfterAlias ->
                    \name ->
                        \commentsAfterName ->
                            \parameters ->
                                \commentsAfterEqual ->
                                    \typeAnnotationResult ->
                                        { comments =
                                            Rope.flatFromList
                                                [ commentsAfterAlias
                                                , commentsAfterName
                                                , parameters.comments
                                                , commentsAfterEqual
                                                , typeAnnotationResult.comments
                                                ]
                                        , syntax =
                                            TypeAliasDeclarationWithoutDocumentation
                                                { name = name
                                                , parameters = parameters.syntax
                                                , typeAnnotation = typeAnnotationResult.syntax
                                                }
                                        }
                )
                Layout.layout
            )
    )
        |= Node.parserCore Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericList
        |. Tokens.equal
        |= Layout.maybeLayout
        |= typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    (Core.map
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
        |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
        |> ParserWithComments.keep typeGenericList
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep valueConstructor
        |> ParserWithComments.keep
            (ParserWithComments.manyWithoutReverse
                (Core.map
                    (\commentsBeforePipe ->
                        \commentsAfterPipe ->
                            \variantResult ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsBeforePipe
                                        , commentsAfterPipe
                                        , variantResult.comments
                                        ]
                                , syntax = variantResult.syntax
                                }
                    )
                    (Layout.maybeLayout |> Core.backtrackable)
                    |. Tokens.pipe
                    |= Layout.maybeLayout
                    |= valueConstructor
                )
            )


valueConstructor : Parser (WithComments (Node ValueConstructor))
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
        |> ParserWithComments.fromCoreKeep
            (ParserWithComments.manyWithoutReverse
                (Core.map
                    (\commentsBefore typeAnnotationResult ->
                        { comments = Rope.flatFromList [ commentsBefore, typeAnnotationResult.comments ]
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    (Layout.maybeLayout |> Core.backtrackable)
                    |= typeAnnotationNoFnExcludingTypedWithArguments
                )
            )


typeGenericList : Parser (WithComments (List (Node String)))
typeGenericList =
    ParserWithComments.many
        (Node.parserCore Tokens.functionName
            |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
        )
