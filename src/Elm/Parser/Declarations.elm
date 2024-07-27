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
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope


declaration : Parser (WithComments (Node Declaration))
declaration =
    Parser.oneOf
        [ (Parser.map
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
                                                |> Parser.succeed

                                        else
                                            Parser.problem
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
                                            |> Parser.succeed

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
                                    |> Parser.succeed

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
                                    |> Parser.succeed

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
                                    |> Parser.succeed
            )
            Comments.declarationDocumentation
            |= Layout.layoutStrict
            |= Parser.oneOf
                [ functionAfterDocumentation
                , typeOrTypeAliasDefinitionAfterDocumentation
                , portDeclarationAfterDocumentation
                ]
          )
            |> Parser.andThen identity
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
    Node.parserCoreMap
        (\startName ->
            \commentsAfterStartName ->
                \maybeSignature ->
                    \arguments ->
                        \commentsAfterEqual ->
                            \result ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterStartName
                                        , case maybeSignature of
                                            Nothing ->
                                                Rope.empty

                                            Just signature ->
                                                signature.comments
                                        , arguments.comments
                                        , commentsAfterEqual
                                        , result.comments
                                        ]
                                , syntax =
                                    FunctionDeclarationAfterDocumentation
                                        { startName = startName
                                        , signature = maybeSignature |> Maybe.map .syntax
                                        , arguments = arguments.syntax
                                        , expression = result.syntax
                                        }
                                }
        )
        Tokens.functionName
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ (Tokens.colon
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBeforeTypeAnnotation ->
                            \typeAnnotationResult ->
                                \commentsAfterTypeAnnotation ->
                                    \implementationNameNode ->
                                        \afterImplementationName ->
                                            Just
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
            , Parser.succeed Nothing
            ]
        |= ParserWithComments.many
            (Parser.map
                (\patternResult ->
                    \commentsAfterPattern ->
                        { comments = Rope.flatFromList [ patternResult.comments, commentsAfterPattern ]
                        , syntax = patternResult.syntax
                        }
                )
                Patterns.pattern
                |= Layout.maybeLayout
            )
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    (Node.parserCoreMap
        (\((Node { start } startName) as startNameNode) ->
            \commentsAfterStartName ->
                \maybeSignature ->
                    \arguments ->
                        \commentsAfterEqual ->
                            \result ->
                                let
                                    allComments : Comments
                                    allComments =
                                        Rope.flatFromList
                                            [ commentsAfterStartName
                                            , case maybeSignature of
                                                Nothing ->
                                                    Rope.empty

                                                Just signature ->
                                                    signature.comments
                                            , arguments.comments
                                            , commentsAfterEqual
                                            , result.comments
                                            ]
                                in
                                case maybeSignature of
                                    Nothing ->
                                        let
                                            (Node expressionRange _) =
                                                result.syntax
                                        in
                                        { comments = allComments
                                        , syntax =
                                            Node { start = start, end = expressionRange.end }
                                                (Declaration.FunctionDeclaration
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Node { start = start, end = expressionRange.end }
                                                            { name = startNameNode
                                                            , arguments = arguments.syntax
                                                            , expression = result.syntax
                                                            }
                                                    }
                                                )
                                        }
                                            |> Parser.succeed

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
                                                Node { start = start, end = expressionRange.end }
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
                                                |> Parser.succeed

                                        else
                                            Parser.problem
                                                ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        Tokens.functionName
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ (Tokens.colon
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBeforeTypeAnnotation ->
                            \typeAnnotationResult ->
                                \commentsAfterTypeAnnotation ->
                                    \implementationNameNode ->
                                        \afterImplementationName ->
                                            Just
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsBeforeTypeAnnotation
                                                        , typeAnnotationResult.comments
                                                        , commentsAfterTypeAnnotation
                                                        , afterImplementationName
                                                        ]
                                                , implementationName = implementationNameNode
                                                , typeAnnotation = typeAnnotationResult.syntax
                                                }
                        )
                        Layout.maybeLayout
                    )
              )
                |= TypeAnnotation.typeAnnotation
                |= Layout.layoutStrict
                |= (Tokens.functionName |> Node.parserCore)
                |= Layout.maybeLayout
            , Parser.succeed Nothing
            ]
        |= ParserWithComments.many
            (Parser.map
                (\patternResult ->
                    \commentsAfterPattern ->
                        { comments = Rope.flatFromList [ patternResult.comments, commentsAfterPattern ]
                        , syntax = patternResult.syntax
                        }
                )
                Patterns.pattern
                |= Layout.maybeLayout
            )
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression
    )
        |> Parser.andThen identity


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    Parser.map
        (\startRow ->
            \commentsAfterInfix ->
                \direction ->
                    \commentsAfterDirection ->
                        \precedence ->
                            \commentsAfterPrecedence ->
                                \operator ->
                                    \commentsAfterOperator ->
                                        \commentsAfterEqual ->
                                            \((Node fnRange _) as fn) ->
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsAfterInfix
                                                        , commentsAfterDirection
                                                        , commentsAfterPrecedence
                                                        , commentsAfterOperator
                                                        , commentsAfterEqual
                                                        ]
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
        Parser.getRow
        |. Parser.keyword "infix"
        |= Layout.layout
        |= Node.parserCore infixDirection
        |= Layout.layout
        |= Node.parserCore Parser.int
        |= Layout.layout
        |= operatorWithParens
        |= Layout.layout
        |. Tokens.equal
        |= Layout.layout
        |= Node.parserCore Tokens.functionName


operatorWithParens : Parser.Parser (Node String)
operatorWithParens =
    (Tokens.parensStart
        |> Parser.Extra.continueWith Tokens.prefixOperatorToken
    )
        |. Tokens.parensEnd
        |> Node.parserCore


infixDirection : Parser.Parser Infix.InfixDirection
infixDirection =
    Parser.oneOf
        [ Parser.keyword "right"
            |> Parser.map (\() -> Infix.Right)
        , Parser.keyword "left"
            |> Parser.map (\() -> Infix.Left)
        , Parser.keyword "non"
            |> Parser.map (\() -> Infix.Non)
        ]


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    Parser.map
        (\startRow ->
            \commentsAfterPort ->
                \name ->
                    \commentsAfterName ->
                        \commentsAfterColon ->
                            \typeAnnotationResult ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterPort
                                        , commentsAfterName
                                        , typeAnnotationResult.comments
                                        , commentsAfterColon
                                        ]
                                , syntax =
                                    PortDeclarationAfterDocumentation
                                        { startLocation = { row = startRow, column = 1 }
                                        , name = name
                                        , typeAnnotation = typeAnnotationResult.syntax
                                        }
                                }
        )
        Parser.getRow
        |. Tokens.portToken
        |= Layout.layout
        |= Node.parserCore Tokens.functionName
        |= Layout.maybeLayout
        |. Parser.symbol ":"
        |= Layout.maybeLayout
        |= typeAnnotation


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    (Parser.map
        (\startRow ->
            \commentsAfterPort ->
                \name ->
                    \commentsAfterName ->
                        \commentsAfterColon ->
                            \typeAnnotationResult ->
                                let
                                    (Node { end } _) =
                                        typeAnnotationResult.syntax
                                in
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterPort
                                        , commentsAfterName
                                        , commentsAfterColon
                                        , typeAnnotationResult.comments
                                        ]
                                , syntax =
                                    Node
                                        { start = { row = startRow, column = 1 }
                                        , end = end
                                        }
                                        (Declaration.PortDeclaration { name = name, typeAnnotation = typeAnnotationResult.syntax })
                                }
        )
        Parser.getRow
        |. Tokens.portToken
        |= Layout.layout
        |= Node.parserCore Tokens.functionName
        |= Layout.maybeLayout
    )
        |. Parser.symbol ":"
        |= Layout.maybeLayout
        |= typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    (Parser.symbol "type"
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterType ->
                    \declarationAfterDocumentation ->
                        { comments = Rope.flatFromList [ commentsAfterType, declarationAfterDocumentation.comments ]
                        , syntax = declarationAfterDocumentation.syntax
                        }
                )
                Layout.layout
            )
    )
        |= Parser.oneOf
            [ typeAliasDefinitionAfterDocumentationAfterTypePrefix
            , customTypeDefinitionAfterDocumentationAfterTypePrefix
            ]


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    (Tokens.aliasToken
        |> Parser.Extra.continueWith
            (Parser.map
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
    Node.parserCoreMap
        (\name ->
            \commentsAfterName ->
                \parameters ->
                    \commentsAfterEqual ->
                        \headVariant ->
                            \tailVariantsReverse ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterName
                                        , parameters.comments
                                        , commentsAfterEqual
                                        , headVariant.comments
                                        , tailVariantsReverse.comments
                                        ]
                                , syntax =
                                    TypeDeclarationAfterDocumentation
                                        { name = name
                                        , parameters = parameters.syntax
                                        , headVariant = headVariant.syntax
                                        , tailVariantsReverse = tailVariantsReverse.syntax
                                        }
                                }
        )
        Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericList
        |. Tokens.equal
        |= Layout.maybeLayout
        |= valueConstructor
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
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
                (Layout.maybeLayout |> Parser.backtrackable)
                |. Tokens.pipe
                |= Layout.maybeLayout
                |= valueConstructor
            )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    Parser.map
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
        Parser.getRow
        |. Parser.symbol "type"
        |= Layout.layout
        |= Parser.oneOf
            [ typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            , customTypeDefinitionWithoutDocumentationAfterTypePrefix
            ]


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    (Tokens.aliasToken
        |> Parser.Extra.continueWith
            (Parser.map
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
    Parser.map
        (\name ->
            \commentsAfterName ->
                \parameters ->
                    \commentsAfterEqual ->
                        \headVariant ->
                            \tailVariantsReverse ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterName
                                        , parameters.comments
                                        , commentsAfterEqual
                                        , headVariant.comments
                                        , tailVariantsReverse.comments
                                        ]
                                , syntax =
                                    TypeDeclarationWithoutDocumentation
                                        { name = name
                                        , parameters = parameters.syntax
                                        , headVariant = headVariant.syntax
                                        , tailVariantsReverse = tailVariantsReverse.syntax
                                        }
                                }
        )
        (Node.parserCore Tokens.typeName)
        |= Layout.maybeLayout
        |= typeGenericList
        |. Tokens.equal
        |= Layout.maybeLayout
        |= valueConstructor
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
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
                (Layout.maybeLayout |> Parser.backtrackable)
                |. Tokens.pipe
                |= Layout.maybeLayout
                |= valueConstructor
            )


valueConstructor : Parser (WithComments (Node ValueConstructor))
valueConstructor =
    (Tokens.typeName
        |> Node.parserCoreMap
            (\((Node variantNameRange _) as variantNameNode) ->
                \argumentsReverse ->
                    let
                        fullEnd : Location
                        fullEnd =
                            case argumentsReverse.syntax of
                                (Node lastArgRange _) :: _ ->
                                    lastArgRange.end

                                [] ->
                                    variantNameRange.end
                    in
                    { comments = argumentsReverse.comments
                    , syntax =
                        Node
                            { start = variantNameRange.start, end = fullEnd }
                            { name = variantNameNode, arguments = List.reverse argumentsReverse.syntax }
                    }
            )
    )
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
                (\commentsBefore typeAnnotationResult ->
                    { comments = Rope.flatFromList [ commentsBefore, typeAnnotationResult.comments ]
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= typeAnnotationNoFnExcludingTypedWithArguments
            )


typeGenericList : Parser (WithComments (List (Node String)))
typeGenericList =
    ParserWithComments.many
        (Parser.map
            (\name ->
                \commentsAfterName ->
                    { comments = commentsAfterName
                    , syntax = name
                    }
            )
            (Node.parserCore Tokens.functionName)
            |= Layout.maybeLayout
        )
