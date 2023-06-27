module Elm.Parser.Expression exposing (expression)

import Elm.Parser.DestructurePatterns as DestructurePatterns
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.DestructurePattern exposing (DestructurePattern)
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    Parser.map
        (\( startRow, startColumn ) ->
            \expressionAndEnd ->
                { comments = expressionAndEnd.comments
                , syntax =
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = expressionAndEnd.end
                        }
                        expressionAndEnd.expression
                }
        )
        Parser.getPosition
        |= Parser.oneOf
            [ qualifiedOrVariantOrRecordConstructorReferenceExpression
            , unqualifiedFunctionReferenceExpression
            , literalExpression
            , numberExpression
            , tupleExpression
            , Tokens.squareStart |> Parser.Extra.continueWith expressionAfterOpeningSquareBracket
            , recordExpression
            , caseExpression
            , lambdaExpression
            , letExpression
            , ifBlockExpression
            , recordAccessFunctionExpression
            , negationOperation
            , charLiteralExpression
            ]


andThenOneOf : List ( Int, Parser (WithComments ExtensionRight) )
andThenOneOf =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ recordAccess
    , infixLeft 1 (Parser.lazy (\() -> abovePrecedence1)) "|>"
    , infixRight 5 (Parser.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (Parser.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (Parser.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (Parser.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (Parser.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (Parser.lazy (\() -> abovePrecedence6)) "+"
    , infixLeftSubtraction 6 (Parser.lazy (\() -> abovePrecedence6))
    , infixLeft 6 (Parser.lazy (\() -> abovePrecedence6)) "|."
    , infixRight 3 (Parser.lazy (\() -> abovePrecedence2)) "&&"
    , infixLeft 5 (Parser.lazy (\() -> abovePrecedence5)) "|="
    , infixLeft 9 (Parser.lazy (\() -> abovePrecedence9)) "<<"
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) "/="
    , infixLeft 7 (Parser.lazy (\() -> abovePrecedence7)) "//"
    , infixLeft 7 (Parser.lazy (\() -> abovePrecedence7)) "/"
    , infixRight 7 (Parser.lazy (\() -> abovePrecedence6)) "</>"
    , infixRight 2 (Parser.lazy (\() -> abovePrecedence1)) "||"
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) "<="
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) ">="
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) ">"
    , infixLeft 8 (Parser.lazy (\() -> abovePrecedence8)) "<?>"
    , infixNonAssociative 4 (Parser.lazy (\() -> abovePrecedence4)) "<"
    , infixRight 8 (Parser.lazy (\() -> abovePrecedence7)) "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser (WithComments (Node Expression))
expression =
    subExpressionMap identity abovePrecedence0


recordAccess : ( Int, Parser (WithComments ExtensionRight) )
recordAccess =
    postfix 100
        recordAccessParser
        (\field ->
            ExtendRightByRecordAccess field
        )


recordAccessParser : Parser (Node String)
recordAccessParser =
    lookBehindOneCharacter
        |> Parser.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    problemRecordAccessStartingWithSpace

                else
                    dotField
            )


problemRecordAccessStartingWithSpace : Parser.Parser a
problemRecordAccessStartingWithSpace =
    Parser.problem "Record access can't start with a space"


dotField : Parser.Parser (Node String)
dotField =
    (Tokens.dot
        |> Parser.Extra.continueWith
            (Parser.map
                (\( nameStartRow, nameStartColumn ) ->
                    \name ->
                        Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                            name
                )
                Parser.getPosition
            )
    )
        |= Tokens.functionName


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (Parser.lazy (\() -> abovePrecedence90))
        Layout.positivelyIndented
        ExtendRightByApplication


glslEnd : String
glslEnd =
    "|]"


glslExpressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
glslExpressionAfterOpeningSquareBracket =
    (Parser.symbol "glsl|"
        |> Parser.Extra.continueWith
            (Parser.mapChompedString
                (\s () ->
                    \( endRow, endColumn ) ->
                        { comments = Rope.empty
                        , end = { row = endRow, column = endColumn }
                        , expression = GLSLExpression s
                        }
                )
                (Parser.chompUntil glslEnd)
            )
    )
        |. Parser.symbol glslEnd
        |= Parser.getPosition


expressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
expressionAfterOpeningSquareBracket =
    Parser.oneOf
        [ glslExpressionAfterOpeningSquareBracket
        , Parser.map
            (\commentsBefore ->
                \maybeElements ->
                    \( endRow, endColumn ) ->
                        case maybeElements of
                            Nothing ->
                                { comments = commentsBefore
                                , end = { row = endRow, column = endColumn }
                                , expression = expressionListEmpty
                                }

                            Just elements ->
                                { comments = commentsBefore |> Rope.prependTo elements.comments
                                , end = { row = endRow, column = endColumn }
                                , expression = ListLiteral elements.syntax
                                }
            )
            Layout.maybeLayout
            |= Parser.oneOf
                [ Parser.map (\() -> Nothing) Tokens.squareEnd
                , Parser.map
                    (\head ->
                        \commentsAfterHead ->
                            \tail ->
                                Just
                                    { comments =
                                        head.comments
                                            |> Rope.prependTo commentsAfterHead
                                            |> Rope.prependTo tail.comments
                                    , syntax = head.syntax :: tail.syntax
                                    }
                    )
                    expression
                    |= Layout.maybeLayout
                    |= ParserWithComments.many
                        (Tokens.comma
                            |> Parser.Extra.continueWith (Layout.maybeAroundBothSides expression)
                        )
                    |. Tokens.squareEnd
                ]
            |= Parser.getPosition
        ]


expressionListEmpty : Expression
expressionListEmpty =
    ListLiteral []



-- recordExpression


recordExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordExpression =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBefore ->
                    \maybeAfterCurly ->
                        \commentsBeforeClosingCurly ->
                            \( endRow, endColumn ) ->
                                case maybeAfterCurly of
                                    Nothing ->
                                        { comments = commentsBefore |> Rope.prependTo commentsBeforeClosingCurly
                                        , end = { row = endRow, column = endColumn }
                                        , expression = expressionRecordEmpty
                                        }

                                    Just afterCurly ->
                                        { comments =
                                            commentsBefore
                                                |> Rope.prependTo afterCurly.comments
                                                |> Rope.prependTo commentsBeforeClosingCurly
                                        , end = { row = endRow, column = endColumn }
                                        , expression = afterCurly.syntax
                                        }
                )
                Layout.maybeLayout
            )
    )
        |= recordContents
        |= Layout.maybeLayout
        |. Tokens.curlyEnd
        |= Parser.getPosition


expressionRecordEmpty : Expression
expressionRecordEmpty =
    RecordExpr []


recordContents : Parser (Maybe (WithComments Expression))
recordContents =
    Parser.oneOf
        [ Parser.map
            (\( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterFunctionName ->
                        \afterNameBeforeFields ->
                            \tailFields ->
                                let
                                    nameNode : Node String
                                    nameNode =
                                        Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                            name
                                in
                                Just
                                    { comments =
                                        commentsAfterFunctionName
                                            |> Rope.prependTo afterNameBeforeFields.comments
                                            |> Rope.prependTo tailFields.comments
                                    , syntax =
                                        case afterNameBeforeFields.syntax of
                                            RecordUpdateFirstSetter firstField ->
                                                RecordUpdateExpression nameNode firstField tailFields.syntax

                                            FieldsFirstValue firstFieldValue ->
                                                RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                                    }
            )
            Parser.getPosition
            |= Tokens.functionName
            |= Layout.maybeLayout
            |= Parser.oneOf
                [ (Tokens.pipe
                    |> Parser.Extra.continueWith
                        (Parser.map
                            (\commentsBefore ->
                                \setterResult ->
                                    { comments = commentsBefore |> Rope.prependTo setterResult.comments
                                    , syntax = RecordUpdateFirstSetter setterResult.syntax
                                    }
                            )
                            Layout.maybeLayout
                        )
                  )
                    |= recordSetterNodeWithLayout
                , (Tokens.equal
                    |> Parser.Extra.continueWith
                        (Parser.map
                            (\commentsBefore ->
                                \expressionResult ->
                                    \commentsAfter ->
                                        { comments =
                                            commentsBefore
                                                |> Rope.prependTo expressionResult.comments
                                                |> Rope.prependTo commentsAfter
                                        , syntax = FieldsFirstValue expressionResult.syntax
                                        }
                            )
                            Layout.maybeLayout
                        )
                  )
                    |= expression
                    |= Layout.maybeLayout
                ]
            |= recordFields
        , Parser.succeed Nothing
        ]


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        ((Tokens.comma
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsBefore ->
                        \setterResult ->
                            { comments = commentsBefore |> Rope.prependTo setterResult.comments
                            , syntax = setterResult.syntax
                            }
                    )
                    Layout.maybeLayout
                )
         )
            |= recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    Parser.map
        (\( nameStartRow, nameStartColumn ) ->
            \name ->
                \commentsAfterFunctionName ->
                    \commentsAfterEquals ->
                        \expressionResult ->
                            \commentsAfterExpression ->
                                \( endRow, endColumn ) ->
                                    let
                                        start : Location
                                        start =
                                            { row = nameStartRow, column = nameStartColumn }
                                    in
                                    { comments =
                                        commentsAfterFunctionName
                                            |> Rope.prependTo commentsAfterEquals
                                            |> Rope.prependTo expressionResult.comments
                                            |> Rope.prependTo commentsAfterExpression
                                    , syntax =
                                        Node { start = start, end = { row = endRow, column = endColumn } }
                                            ( Node.singleLineStringFrom start
                                                name
                                            , expressionResult.syntax
                                            )
                                    }
        )
        Parser.getPosition
        |= Tokens.functionName
        |= Layout.maybeLayout
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        |= Parser.getPosition


literalExpression : Parser { comments : Comments, end : Location, expression : Expression }
literalExpression =
    Parser.map
        (\( stringLiteralType, string ) ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = StringLiteral stringLiteralType string
                }
        )
        Tokens.singleOrTripleQuotedStringLiteral
        |= Parser.getPosition


charLiteralExpression : Parser { comments : Comments, end : Location, expression : Expression }
charLiteralExpression =
    Parser.map
        (\char ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = CharLiteral char
                }
        )
        Tokens.characterLiteral
        |= Parser.getPosition



-- lambda


lambdaExpression : Parser { comments : Comments, end : Location, expression : Expression }
lambdaExpression =
    (Tokens.backSlash
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterBackslash ->
                    \firstArg ->
                        \commentsAfterFirstArg ->
                            \secondUpArgs ->
                                \expressionResult ->
                                    let
                                        (Node { end } _) =
                                            expressionResult.syntax
                                    in
                                    { comments =
                                        commentsAfterBackslash
                                            |> Rope.prependTo firstArg.comments
                                            |> Rope.prependTo commentsAfterFirstArg
                                            |> Rope.prependTo secondUpArgs.comments
                                            |> Rope.prependTo expressionResult.comments
                                    , end = end
                                    , expression =
                                        { firstArg = firstArg.syntax
                                        , restOfArgs = secondUpArgs.syntax
                                        , expression = expressionResult.syntax
                                        }
                                            |> LambdaExpression
                                    }
                )
                Layout.maybeLayout
            )
    )
        |= DestructurePatterns.destructurePattern
        |= Layout.maybeLayout
        |= ParserWithComments.until Tokens.arrowRight
            (Parser.map
                (\patternResult ->
                    \commentsAfter ->
                        { comments =
                            patternResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = patternResult.syntax
                        }
                )
                DestructurePatterns.destructurePattern
                |= Layout.maybeLayout
            )
        |= expression



-- Case Expression


caseExpression : Parser { comments : Comments, end : Location, expression : Expression }
caseExpression =
    (Tokens.caseToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterCase ->
                    \casedExpressionResult ->
                        \commentsBeforeOf ->
                            \commentsAfterOf ->
                                \casesResult ->
                                    let
                                        ( ( _, Node firstCaseExpressionRange _ ) as firstCase, lastToSecondCase ) =
                                            casesResult.syntax
                                    in
                                    { comments =
                                        commentsAfterCase
                                            |> Rope.prependTo casedExpressionResult.comments
                                            |> Rope.prependTo commentsBeforeOf
                                            |> Rope.prependTo commentsAfterOf
                                            |> Rope.prependTo casesResult.comments
                                    , end =
                                        case lastToSecondCase of
                                            [] ->
                                                firstCaseExpressionRange.end

                                            ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                                lastCaseExpressionRange.end
                                    , expression =
                                        CaseExpression
                                            { expression = casedExpressionResult.syntax
                                            , firstCase = firstCase
                                            , restOfCases = List.reverse lastToSecondCase
                                            }
                                    }
                )
                Layout.maybeLayout
            )
    )
        |= expression
        |= Layout.maybeLayout
        |. Tokens.ofToken
        |= Layout.maybeLayout
        |= Parser.Extra.withIndent caseStatements


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    Parser.map
        (\firstCasePatternResult ->
            \commentsAfterFirstCasePattern ->
                \commentsAfterFirstCaseArrowRight ->
                    \firstCaseExpressionResult ->
                        \lastToSecondCase ->
                            { comments =
                                firstCasePatternResult.comments
                                    |> Rope.prependTo commentsAfterFirstCasePattern
                                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                                    |> Rope.prependTo firstCaseExpressionResult.comments
                                    |> Rope.prependTo lastToSecondCase.comments
                            , syntax =
                                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                                , lastToSecondCase.syntax
                                )
                            }
        )
        Patterns.pattern
        |= Layout.maybeLayout
        |. Tokens.arrowRight
        |= Layout.maybeLayout
        |= expression
        |= ParserWithComments.manyWithoutReverse caseStatement


caseStatement : Parser (WithComments Case)
caseStatement =
    Parser.map
        (\commentsBeforeCase ->
            \pattern ->
                \commentsBeforeArrowRight ->
                    \commentsAfterArrowRight ->
                        \expr ->
                            { comments =
                                commentsBeforeCase
                                    |> Rope.prependTo pattern.comments
                                    |> Rope.prependTo commentsBeforeArrowRight
                                    |> Rope.prependTo commentsAfterArrowRight
                                    |> Rope.prependTo expr.comments
                            , syntax = ( pattern.syntax, expr.syntax )
                            }
        )
        (Layout.optimisticLayout |> Parser.backtrackable)
        |. Layout.onTopIndentation ()
        |= Patterns.pattern
        |= Layout.maybeLayout
        |. Tokens.arrowRight
        |= Layout.maybeLayout
        |= expression



-- Let Expression


letExpression : Parser { comments : Comments, end : Location, expression : Expression }
letExpression =
    Parser.Extra.withIndent
        ((Tokens.letToken
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsAfterLet ->
                        \declarations ->
                            \commentsAfterIn ->
                                \expressionResult ->
                                    let
                                        ((Node { end } _) as expr) =
                                            expressionResult.syntax
                                    in
                                    { comments =
                                        commentsAfterLet
                                            |> Rope.prependTo declarations.comments
                                            |> Rope.prependTo commentsAfterIn
                                            |> Rope.prependTo expressionResult.comments
                                    , end = end
                                    , expression = LetExpression { declarations = declarations.syntax, expression = expr }
                                    }
                    )
                    Layout.maybeLayout
                )
         )
            |= Parser.Extra.withIndent letDeclarationsIn
        )
        -- check that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        |. Layout.positivelyIndentedPlus 2
        |= Layout.maybeLayout
        |= expression


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentation
        (\headLetResult ->
            \commentsAfter ->
                \tailLetResult ->
                    { comments =
                        headLetResult.comments
                            |> Rope.prependTo commentsAfter
                            |> Rope.prependTo tailLetResult.comments
                    , syntax = headLetResult.syntax :: tailLetResult.syntax
                    }
        )
        |= Parser.oneOf
            [ letFunction
            , letDestructuringDeclaration
            ]
        |= Layout.optimisticLayout
        |= ParserWithComments.until Tokens.inToken blockElement


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentation
        (\letDeclarationResult ->
            \commentsAfter ->
                { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
                , syntax = letDeclarationResult.syntax
                }
        )
        |= Parser.oneOf
            [ letFunction
            , letDestructuringDeclaration
            ]
        |= Layout.optimisticLayout


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    Parser.map
        (\pattern ->
            \expressionResult ->
                let
                    (Node { start } _) =
                        pattern.syntax

                    (Node { end } _) =
                        expressionResult.syntax
                in
                { comments = pattern.comments |> Rope.prependTo expressionResult.comments
                , syntax =
                    Node { start = start, end = end }
                        (LetDestructuring pattern.syntax expressionResult.syntax)
                }
        )
        DestructurePatterns.destructurePattern
        |. Tokens.equal
        |= expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    Parser.map
        (\( startNameStartRow, startNameStartColumn ) ->
            \startName ->
                \commentsAfterStartName ->
                    \maybeSignature ->
                        \arguments ->
                            \commentsAfterEqual ->
                                \expressionResult ->
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
                                                |> Rope.prependTo expressionResult.comments

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
                                                    expressionResult.syntax
                                            in
                                            { comments = allComments
                                            , syntax =
                                                Node { start = start, end = expressionRange.end }
                                                    (LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            Node { start = start, end = expressionRange.end }
                                                                { name = startNameNode
                                                                , arguments = arguments.syntax
                                                                , expression = expressionResult.syntax
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
                                                        expressionResult.syntax
                                                in
                                                { comments = allComments
                                                , syntax =
                                                    Node { start = start, end = expressionRange.end }
                                                        (LetFunction
                                                            { documentation = Nothing
                                                            , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                                            , declaration =
                                                                Node { start = implementationNameRange.start, end = expressionRange.end }
                                                                    { name = signature.implementationName
                                                                    , arguments = arguments.syntax
                                                                    , expression = expressionResult.syntax
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


parameterPatternsEqual : Parser (WithComments (List (Node DestructurePattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (Parser.map
            (\patternResult ->
                \commentsAfterPattern ->
                    { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                    , syntax = patternResult.syntax
                    }
            )
            DestructurePatterns.destructurePattern
            |= Layout.maybeLayout
        )


numberExpression : Parser { comments : Comments, end : Location, expression : Expression }
numberExpression =
    Elm.Parser.Numbers.forgivingNumber
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = FloatLiteral n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = IntegerLiteral n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = HexLiteral n
                }
        )
        |= Parser.getPosition


ifBlockExpression : Parser { comments : Comments, end : Location, expression : Expression }
ifBlockExpression =
    (Tokens.ifToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterIf ->
                    \condition ->
                        \commentsBeforeThen ->
                            \commentsAfterThen ->
                                \ifTrue ->
                                    \commentsBeforeElse ->
                                        \commentsAfterElse ->
                                            \ifFalse ->
                                                let
                                                    (Node { end } _) =
                                                        ifFalse.syntax
                                                in
                                                { comments =
                                                    commentsAfterIf
                                                        |> Rope.prependTo condition.comments
                                                        |> Rope.prependTo commentsBeforeThen
                                                        |> Rope.prependTo commentsAfterThen
                                                        |> Rope.prependTo ifTrue.comments
                                                        |> Rope.prependTo commentsBeforeElse
                                                        |> Rope.prependTo commentsAfterElse
                                                        |> Rope.prependTo ifFalse.comments
                                                , end = end
                                                , expression = If condition.syntax ifTrue.syntax ifFalse.syntax
                                                }
                )
                Layout.maybeLayout
            )
    )
        |= expression
        |= Layout.maybeLayout
        |. Tokens.thenToken
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayout
        |. Tokens.elseToken
        |= Layout.maybeLayout
        |= expression


negationOperation : Parser { comments : Comments, end : Location, expression : Expression }
negationOperation =
    minusNotFollowedBySpace
        |> Parser.Extra.continueWith
            (subExpressionMap
                (\subExpressionResult ->
                    let
                        (Node { end } _) =
                            subExpressionResult.syntax
                    in
                    { comments = subExpressionResult.comments
                    , end = end
                    , expression = Negation subExpressionResult.syntax
                    }
                )
                abovePrecedence95
            )


minusNotFollowedBySpace : Parser.Parser ()
minusNotFollowedBySpace =
    Parser.backtrackable Tokens.minus
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ Parser.chompIf (\next -> next == '\u{000D}' || next == '\n' || next == ' ')
                    |> Parser.backtrackable
                    |> Parser.map (\() -> problemNegationThenSpace)
                , Parser.succeed (Parser.commit ())
                ]
            )
        |> Parser.andThen identity


problemNegationThenSpace : Parser.Parser a
problemNegationThenSpace =
    Parser.problem "negation sign cannot be followed by a space"


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    Parser.map
        (\firstName ->
            \after ->
                \( endRow, endColumn ) ->
                    { comments = Rope.empty
                    , end = { row = endRow, column = endColumn }
                    , expression =
                        case after of
                            Nothing ->
                                FunctionOrValue [] firstName

                            Just ( qualificationAfter, unqualified ) ->
                                FunctionOrValue (firstName :: qualificationAfter) unqualified
                    }
        )
        Tokens.typeName
        |= maybeDotReferenceExpressionTuple
        |= Parser.getPosition


unqualifiedFunctionReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
unqualifiedFunctionReferenceExpression =
    Parser.map
        (\unqualified ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = FunctionOrValue [] unqualified
                }
        )
        Tokens.functionName
        |= Parser.getPosition


maybeDotReferenceExpressionTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    Parser.oneOf
        [ Tokens.dot
            |> Parser.Extra.continueWith
                (Parser.oneOf
                    [ Parser.map
                        (\firstName ->
                            \after ->
                                case after of
                                    Nothing ->
                                        Just ( [], firstName )

                                    Just ( qualificationAfter, unqualified ) ->
                                        Just ( firstName :: qualificationAfter, unqualified )
                        )
                        Tokens.typeName
                        |= Parser.lazy (\() -> maybeDotReferenceExpressionTuple)
                    , Parser.map (\unqualified -> Just ( [], unqualified )) Tokens.functionName
                    ]
                )
        , Parser.succeed Nothing
        ]


recordAccessFunctionExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordAccessFunctionExpression =
    Tokens.dot
        |> Parser.Extra.continueWith
            (Parser.map
                (\field ->
                    \( endRow, endColumn ) ->
                        { comments = Rope.empty
                        , end = { row = endRow, column = endColumn }
                        , expression = RecordAccessFunction field
                        }
                )
                Tokens.functionName
                |= Parser.getPosition
            )


tupleExpression : Parser { comments : Comments, end : Location, expression : Expression }
tupleExpression =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                ((Tokens.parensEnd
                    |> Parser.Extra.continueWith
                        (Parser.map
                            (\( endRow, endColumn ) ->
                                { comments = Rope.empty
                                , end = { row = endRow, column = endColumn }
                                , expression = TupleExpression []
                                }
                            )
                            Parser.getPosition
                        )
                 )
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (Parser.symbol "-)"
                            |> Parser.Extra.continueWith
                                (Parser.map
                                    (\( endRow, endColumn ) ->
                                        { comments = Rope.empty
                                        , end = { row = endRow, column = endColumn }
                                        , expression = expressionPrefixOperatorMinus
                                        }
                                    )
                                    Parser.getPosition
                                )
                       )
                    :: tupledExpressionInnerAfterOpeningParens
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )


expressionPrefixOperatorMinus : Expression
expressionPrefixOperatorMinus =
    PrefixOperator "-"


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser { comments : Comments, end : Location, expression : Expression })
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                Parser.symbol (allowedOperatorToken ++ ")")
                    |> Parser.Extra.continueWith
                        (Parser.map
                            (\( endRow, endColumn ) ->
                                { comments = Rope.empty
                                , end = { row = endRow, column = endColumn }
                                , expression = PrefixOperator allowedOperatorToken
                                }
                            )
                            Parser.getPosition
                        )
            )


tupledExpressionInnerAfterOpeningParens : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpressionInnerAfterOpeningParens =
    Parser.map
        (\firstPart ->
            \commentsAfterFirstPart ->
                \tailPartsReverse ->
                    \( endRow, endColumn ) ->
                        case tailPartsReverse.syntax of
                            [] ->
                                { comments = firstPart.comments |> Rope.prependTo commentsAfterFirstPart
                                , end = { row = endRow, column = endColumn }
                                , expression = TupleExpression [ firstPart.syntax ]
                                }

                            _ ->
                                { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
                                , end = { row = endRow, column = endColumn }
                                , expression = TupleExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                                }
        )
        expression
        |= Layout.maybeLayout
        |= ParserWithComments.untilWithoutReverse Tokens.parensEnd
            (Tokens.comma
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBefore ->
                            \partResult ->
                                \commentsAfter ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo partResult.comments
                                            |> Rope.prependTo commentsAfter
                                    , syntax = partResult.syntax
                                    }
                        )
                        Layout.maybeLayout
                        |= expression
                        |= Layout.maybeLayout
                    )
            )
        |= Parser.getPosition



---


subExpressionMap :
    (WithComments (Node Expression) -> a)
    -> Parser (WithComments ExtensionRight)
    -> Parser a
subExpressionMap toExtensionRightWith aboveCurrentPrecedenceLayout =
    let
        step : WithComments (Node Expression) -> Parser (Parser.Step (WithComments (Node Expression)) a)
        step leftExpressionResult =
            Parser.oneOf
                [ Parser.map
                    (\extensionRight ->
                        \commentsAfter ->
                            { comments =
                                leftExpressionResult.comments
                                    |> Rope.prependTo extensionRight.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax =
                                leftExpressionResult.syntax
                                    |> applyExtensionRight extensionRight.syntax
                            }
                                |> Parser.Loop
                    )
                    aboveCurrentPrecedenceLayout
                    |= Layout.optimisticLayout
                , Parser.succeed
                    (Parser.Done (toExtensionRightWith leftExpressionResult))
                ]
    in
    Parser.map
        (\commentsBefore ->
            \leftExpressionResult ->
                \commentsAfter ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo leftExpressionResult.comments
                            |> Rope.prependTo commentsAfter
                    , syntax = leftExpressionResult.syntax
                    }
        )
        Layout.optimisticLayout
        |= Parser.lazy (\() -> subExpression)
        |= Layout.optimisticLayout
        |> Parser.andThen
            (\leftExpression -> Parser.loop leftExpression step)


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight extensionRight ((Node { start } left) as leftNode) =
    case extensionRight of
        ExtendRightByRecordAccess ((Node { end } _) as field) ->
            Node { start = start, end = end }
                (Expression.RecordAccess leftNode field)

        ExtendRightByApplication ((Node { end } _) as right) ->
            Node { start = start, end = end }
                (case left of
                    Expression.Application called (firstArg :: secondArgUp) ->
                        Expression.Application called (firstArg :: (secondArgUp ++ [ right ]))

                    _ ->
                        Expression.Application leftNode [ right ]
                )

        ExtendRightByOperation extendRightOperation ->
            let
                ((Node { end } _) as right) =
                    extendRightOperation.expression
            in
            Node { start = start, end = end }
                (Operation extendRightOperation.symbol extendRightOperation.direction leftNode right)


abovePrecedence0 : Parser (WithComments ExtensionRight)
abovePrecedence0 =
    computeAbovePrecedence 0


abovePrecedence1 : Parser (WithComments ExtensionRight)
abovePrecedence1 =
    computeAbovePrecedence 1


abovePrecedence2 : Parser (WithComments ExtensionRight)
abovePrecedence2 =
    computeAbovePrecedence 2


abovePrecedence4 : Parser (WithComments ExtensionRight)
abovePrecedence4 =
    computeAbovePrecedence 4


abovePrecedence5 : Parser (WithComments ExtensionRight)
abovePrecedence5 =
    computeAbovePrecedence 5


abovePrecedence6 : Parser (WithComments ExtensionRight)
abovePrecedence6 =
    computeAbovePrecedence 6


abovePrecedence7 : Parser (WithComments ExtensionRight)
abovePrecedence7 =
    computeAbovePrecedence 7


abovePrecedence8 : Parser (WithComments ExtensionRight)
abovePrecedence8 =
    computeAbovePrecedence 8


abovePrecedence9 : Parser (WithComments ExtensionRight)
abovePrecedence9 =
    computeAbovePrecedence 9


abovePrecedence90 : Parser (WithComments ExtensionRight)
abovePrecedence90 =
    computeAbovePrecedence 90


abovePrecedence95 : Parser (WithComments ExtensionRight)
abovePrecedence95 =
    computeAbovePrecedence 95


computeAbovePrecedence : Int -> Parser (WithComments ExtensionRight)
computeAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )
        |> Parser.oneOf


infixLeft : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixLeft precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (Parser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Left, expression = right }
        )


infixNonAssociative : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociative precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (Parser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Non, expression = right }
        )


{-| To get right associativity, please provide abovePrecedence(precedence-1) for the
right precedence parser.
-}
infixRight : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixRight precedence possibilitiesForPrecedenceMinus1 symbol =
    infixHelp precedence
        possibilitiesForPrecedenceMinus1
        (Parser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Right, expression = right }
        )


lookBehindOneCharacter : Parser.Parser String
lookBehindOneCharacter =
    Parser.map (\offset -> \source -> String.slice (offset - 1) offset source)
        Parser.getOffset
        |= Parser.getSource


infixLeftSubtraction : Int -> Parser (WithComments ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
infixLeftSubtraction precedence possibilitiesForPrecedence =
    infixHelp precedence
        possibilitiesForPrecedence
        (lookBehindOneCharacter
            |> Parser.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Tokens.minusSymbols

                    else
                        Tokens.minus
                )
        )
        (\right ->
            ExtendRightByOperation { symbol = "-", direction = Infix.Left, expression = right }
        )


infixHelp :
    Int
    -> Parser (WithComments ExtensionRight)
    -> Parser.Parser ()
    -> (Node Expression -> ExtensionRight)
    -> ( Int, Parser (WithComments ExtensionRight) )
infixHelp leftPrecedence rightPrecedence operator apply =
    ( leftPrecedence
    , operator
        |> Parser.Extra.continueWith
            (subExpressionMap
                (\e ->
                    { comments = e.comments
                    , syntax = apply e.syntax
                    }
                )
                rightPrecedence
            )
    )


postfix : Int -> Parser a -> (a -> ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
postfix precedence operator apply =
    ( precedence
    , Parser.map
        (\right ->
            { comments = Rope.empty
            , syntax = apply right
            }
        )
        operator
    )


type ExtensionRight
    = ExtendRightByOperation { symbol : String, direction : Infix.InfixDirection, expression : Node Expression }
    | ExtendRightByApplication (Node Expression)
    | ExtendRightByRecordAccess (Node String)
