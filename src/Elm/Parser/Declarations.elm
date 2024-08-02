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
import Elm.Syntax.Range exposing (Location, Range)
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
        [ functionDeclarationWithoutDocumentation
        , declarationWithDocumentation
        , typeOrTypeAliasDefinitionWithoutDocumentation
        , portDeclarationWithoutDocumentation
        , infixDeclaration
        ]


declarationWithDocumentation : Parser (WithComments (Node Declaration))
declarationWithDocumentation =
    Parser.map
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
                                |> Parser.succeed

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
                                |> Parser.succeed

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
                                |> Parser.succeed
        )
        Comments.declarationDocumentation
        |= Layout.layoutStrict
        |= Parser.oneOf
            [ functionAfterDocumentation
            , typeOrTypeAliasDefinitionAfterDocumentation
            , portDeclarationAfterDocumentation
            ]
        |> Parser.andThen identity


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
    Parser.map
        (\( startNameStartRow, startNameStartColumn ) ->
            \startName ->
                \commentsAfterStartName ->
                    \maybeSignature ->
                        \arguments ->
                            \commentsAfterEqual ->
                                \result ->
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
                                                Node.singleLineStringFrom { row = startNameStartRow, column = startNameStartColumn }
                                                    startName
                                            , signature = maybeSignature |> Maybe.map .syntax
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
        )
        Parser.getPosition
        -- infix declarations itself don't have documentation
        |= Tokens.functionName
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ (Tokens.colon
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBeforeTypeAnnotation ->
                            \typeAnnotationResult ->
                                \commentsAfterTypeAnnotation ->
                                    \( implementationNameStartRow, implementationNameStartColumn ) ->
                                        \implementationName ->
                                            \afterImplementationName ->
                                                Just
                                                    { comments =
                                                        commentsBeforeTypeAnnotation
                                                            |> Rope.prependTo typeAnnotationResult.comments
                                                            |> Rope.prependTo commentsAfterTypeAnnotation
                                                            |> Rope.prependTo afterImplementationName
                                                    , syntax =
                                                        { implementationName =
                                                            Node.singleLineStringFrom { row = implementationNameStartRow, column = implementationNameStartColumn }
                                                                implementationName
                                                        , typeAnnotation = typeAnnotationResult.syntax
                                                        }
                                                    }
                        )
                        Layout.maybeLayout
                    )
              )
                |= TypeAnnotation.typeAnnotation
                |= Layout.layoutStrict
                |= Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayout
            , Parser.succeed Nothing
            ]
        |= parameterPatternsEqual
        |= Layout.maybeLayout
        |= expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    Parser.map
        (\( startNameStartRow, startNameStartColumn ) ->
            \startName ->
                \commentsAfterStartName ->
                    \maybeSignature ->
                        \arguments ->
                            \commentsAfterEqual ->
                                \result ->
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

                                        start : Location
                                        start =
                                            { row = startNameStartRow, column = startNameStartColumn }

                                        startNameNode : Node String
                                        startNameNode =
                                            Node.singleLineStringFrom start
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
        Parser.getPosition
        |= Tokens.functionNameNotInfix
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ (Tokens.colon
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBeforeTypeAnnotation ->
                            \typeAnnotationResult ->
                                \commentsAfterTypeAnnotation ->
                                    \( implementationNameStartRow, implementationNameStartColumn ) ->
                                        \implementationName ->
                                            \afterImplementationName ->
                                                Just
                                                    { comments =
                                                        commentsBeforeTypeAnnotation
                                                            |> Rope.prependTo typeAnnotationResult.comments
                                                            |> Rope.prependTo commentsAfterTypeAnnotation
                                                            |> Rope.prependTo afterImplementationName
                                                    , implementationName =
                                                        Node.singleLineStringFrom { row = implementationNameStartRow, column = implementationNameStartColumn }
                                                            implementationName
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                        )
                        Layout.maybeLayout
                    )
              )
                |= TypeAnnotation.typeAnnotation
                |= Layout.layoutStrict
                |= Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayout
            , Parser.succeed Nothing
            ]
        |= parameterPatternsEqual
        |= Layout.maybeLayout
        |= expression
        |> Parser.andThen identity


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (Parser.map
            (\patternResult ->
                \commentsAfterPattern ->
                    { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                    , syntax = patternResult.syntax
                    }
            )
            Patterns.pattern
            |= Layout.maybeLayout
        )


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    Parser.keyword "infix"
        |> Parser.Extra.continueWith
            (Parser.map
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
                Parser.getRow
                |= Layout.maybeLayout
                |= Node.parserCore infixDirection
                |= Layout.maybeLayout
                |= Node.parserCore Parser.int
                |= Layout.maybeLayout
                |= Node.parserCore
                    ((Tokens.parensStart
                        |> Parser.Extra.continueWith Tokens.prefixOperatorToken
                     )
                        |. Tokens.parensEnd
                    )
                |= Layout.maybeLayoutUntilIgnored Parser.token "="
                |= Layout.maybeLayout
                |= Node.parserCore Tokens.functionName
            )


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
    Tokens.portToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\startRow ->
                    \commentsAfterPort ->
                        \( nameStartRow, nameStartColumn ) ->
                            \name ->
                                \commentsAfterName ->
                                    \commentsAfterColon ->
                                        \typeAnnotationResult ->
                                            { comments =
                                                commentsAfterPort
                                                    |> Rope.prependTo commentsAfterName
                                                    |> Rope.prependTo typeAnnotationResult.comments
                                                    |> Rope.prependTo commentsAfterColon
                                            , syntax =
                                                PortDeclarationAfterDocumentation
                                                    { startLocation = { row = startRow, column = 1 }
                                                    , name =
                                                        Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                            name
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                                            }
                )
                Parser.getRow
                |= Layout.maybeLayout
                |= Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayoutUntilIgnored Parser.token ":"
                |= Layout.maybeLayout
                |= typeAnnotation
            )


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    Tokens.portToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\startRow ->
                    \commentsAfterPort ->
                        \( nameStartRow, nameStartColumn ) ->
                            \name ->
                                \commentsAfterName ->
                                    \commentsAfterColon ->
                                        \typeAnnotationResult ->
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
                                                            Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                                name
                                                        , typeAnnotation = typeAnnotationResult.syntax
                                                        }
                                                    )
                                            }
                )
                Parser.getRow
                |= Layout.maybeLayout
                |= Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayoutUntilIgnored Parser.token ":"
                |= Layout.maybeLayout
                |= typeAnnotation
            )


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    (Parser.keyword "type"
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterType ->
                    \declarationAfterDocumentation ->
                        { comments = commentsAfterType |> Rope.prependTo declarationAfterDocumentation.comments
                        , syntax = declarationAfterDocumentation.syntax
                        }
                )
                Layout.maybeLayout
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
                    \( nameStartRow, nameStartColumn ) ->
                        \name ->
                            \commentsAfterName ->
                                \parameters ->
                                    \commentsAfterEquals ->
                                        \typeAnnotationResult ->
                                            { comments =
                                                commentsAfterAlias
                                                    |> Rope.prependTo commentsAfterName
                                                    |> Rope.prependTo parameters.comments
                                                    |> Rope.prependTo commentsAfterEquals
                                                    |> Rope.prependTo typeAnnotationResult.comments
                                            , syntax =
                                                TypeAliasDeclarationAfterDocumentation
                                                    { name =
                                                        Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                            name
                                                    , parameters = parameters.syntax
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                                            }
                )
                Layout.maybeLayout
            )
    )
        |= Parser.getPosition
        |= Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericListEquals
        |= Layout.maybeLayout
        |= typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    Parser.map
        (\( nameStartRow, nameStartColumn ) ->
            \name ->
                \commentsAfterName ->
                    \parameters ->
                        \commentsAfterEqual ->
                            \headVariant ->
                                \tailVariantsReverse ->
                                    { comments =
                                        commentsAfterName
                                            |> Rope.prependTo parameters.comments
                                            |> Rope.prependTo commentsAfterEqual
                                            |> Rope.prependTo headVariant.comments
                                            |> Rope.prependTo tailVariantsReverse.comments
                                    , syntax =
                                        TypeDeclarationAfterDocumentation
                                            { name =
                                                Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                    name
                                            , parameters = parameters.syntax
                                            , headVariant = headVariant.syntax
                                            , tailVariantsReverse = tailVariantsReverse.syntax
                                            }
                                    }
        )
        Parser.getPosition
        |= Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericListEquals
        |= Layout.maybeLayout
        |= valueConstructor
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
                (\commentsBeforePipe ->
                    \commentsAfterPipe ->
                        \variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo commentsAfterPipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                )
                (Layout.maybeLayoutUntilIgnored Parser.token "|" |> Parser.backtrackable)
                |= Layout.maybeLayout
                |= valueConstructor
            )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    Parser.keyword "type"
        |> Parser.Extra.continueWith
            (Parser.map
                (\startRow ->
                    let
                        start : Location
                        start =
                            { row = startRow, column = 1 }
                    in
                    \commentsAfterType ->
                        \afterStart ->
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
                Parser.getRow
                |= Layout.maybeLayout
                |= Parser.oneOf
                    [ typeAliasDefinitionWithoutDocumentationAfterTypePrefix
                    , customTypeDefinitionWithoutDocumentationAfterTypePrefix
                    ]
            )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    (Tokens.aliasToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterAlias ->
                    \( nameStartRow, nameStartColumn ) ->
                        \name ->
                            \commentsAfterName ->
                                \parameters ->
                                    \commentsAfterEqual ->
                                        \typeAnnotationResult ->
                                            { comments =
                                                commentsAfterAlias
                                                    |> Rope.prependTo commentsAfterName
                                                    |> Rope.prependTo parameters.comments
                                                    |> Rope.prependTo commentsAfterEqual
                                                    |> Rope.prependTo typeAnnotationResult.comments
                                            , syntax =
                                                TypeAliasDeclarationWithoutDocumentation
                                                    { name =
                                                        Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                            name
                                                    , parameters = parameters.syntax
                                                    , typeAnnotation = typeAnnotationResult.syntax
                                                    }
                                            }
                )
                Layout.maybeLayout
            )
    )
        |= Parser.getPosition
        |= Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericListEquals
        |= Layout.maybeLayout
        |= typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    Parser.map
        (\( nameStartRow, nameStartColumn ) ->
            \name ->
                \commentsAfterName ->
                    \parameters ->
                        \commentsAfterEqual ->
                            \headVariant ->
                                \tailVariantsReverse ->
                                    { comments =
                                        commentsAfterName
                                            |> Rope.prependTo parameters.comments
                                            |> Rope.prependTo commentsAfterEqual
                                            |> Rope.prependTo headVariant.comments
                                            |> Rope.prependTo tailVariantsReverse.comments
                                    , syntax =
                                        TypeDeclarationWithoutDocumentation
                                            { name =
                                                Node.singleLineStringFrom
                                                    { row = nameStartRow, column = nameStartColumn }
                                                    name
                                            , parameters = parameters.syntax
                                            , headVariant = headVariant.syntax
                                            , tailVariantsReverse = tailVariantsReverse.syntax
                                            }
                                    }
        )
        Parser.getPosition
        |= Tokens.typeName
        |= Layout.maybeLayout
        |= typeGenericListEquals
        |= Layout.maybeLayout
        |= valueConstructor
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
                (\commentsBeforePipe ->
                    \commentsAfterPipe ->
                        \variantResult ->
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo commentsAfterPipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
                )
                (Layout.maybeLayoutUntilIgnored Parser.token "|" |> Parser.backtrackable)
                |= Layout.maybeLayout
                |= valueConstructor
            )


valueConstructor : Parser (WithComments (Node ValueConstructor))
valueConstructor =
    Parser.map
        (\( nameStartRow, nameStartColumn ) ->
            \name ->
                \argumentsReverse ->
                    let
                        nameStart : Location
                        nameStart =
                            { row = nameStartRow, column = nameStartColumn }

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
        Parser.getPosition
        |= Tokens.typeName
        |= ParserWithComments.manyWithoutReverse
            (Parser.map
                (\commentsBefore typeAnnotationResult ->
                    { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= typeAnnotationNoFnExcludingTypedWithArguments
            )


typeGenericListEquals : Parser (WithComments (List (Node String)))
typeGenericListEquals =
    ParserWithComments.until Tokens.equal
        (Parser.map
            (\( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterName ->
                        { comments = commentsAfterName
                        , syntax =
                            Node.singleLineStringFrom
                                { row = nameStartRow, column = nameStartColumn }
                                name
                        }
            )
            Parser.getPosition
            |= Tokens.functionName
            |= Layout.maybeLayout
        )
