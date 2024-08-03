module Elm.Parser.Expression exposing (expression)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Parser exposing ((|.), (|=), Parser)
import Parser.Advanced
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    ParserFast.oneOf
        [ qualifiedOrVariantOrRecordConstructorReferenceExpression
        , unqualifiedFunctionReferenceExpression
        , literalExpression
        , numberExpression
        , tupledExpression
        , listOrGlslExpression
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
    , infixLeft 1 (ParserFast.lazy (\() -> abovePrecedence1)) "|>"
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (ParserFast.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (ParserFast.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "+"
    , infixLeftSubtraction 6 (ParserFast.lazy (\() -> abovePrecedence6))
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "|."
    , infixRight 3 (ParserFast.lazy (\() -> abovePrecedence2)) "&&"
    , infixLeft 5 (ParserFast.lazy (\() -> abovePrecedence5)) "|="
    , infixLeft 9 (ParserFast.lazy (\() -> abovePrecedence9)) "<<"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "/="
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "//"
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "/"
    , infixRight 7 (ParserFast.lazy (\() -> abovePrecedence6)) "</>"
    , infixRight 2 (ParserFast.lazy (\() -> abovePrecedence1)) "||"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "<="
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) ">="
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) ">"
    , infixLeft 8 (ParserFast.lazy (\() -> abovePrecedence8)) "<?>"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "<"
    , infixRight 8 (ParserFast.lazy (\() -> abovePrecedence7)) "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser (WithComments (Node Expression))
expression =
    subExpressionMap abovePrecedence0


recordAccess : ( Int, Parser (WithComments ExtensionRight) )
recordAccess =
    postfix 100
        recordAccessParser
        ExtendRightByRecordAccess


problemRecordAccessStartingWithSpace : ParserFast.Parser a
problemRecordAccessStartingWithSpace =
    ParserFast.problem "Record access can't start with a space"


dotField : ParserFast.Parser (WithComments ExtensionRight)
dotField =
    Parser.map
        (\() ->
            \( nameStartRow, nameStartColumn ) ->
                \name ->
                    Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                        name
        )
        Tokens.dot
        |= Parser.getPosition
        |= Tokens.functionName


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (Parser.lazy (\() -> abovePrecedence90))
        (Layout.positivelyIndented ())
        ExtendRightByApplication


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithStartAndEndPosition
            (\start s end ->
                { comments = Rope.empty
                , syntax =
                    Node
                        -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                        { start = { row = start.row, column = start.column - 6 }
                        , end = { row = end.row, column = end.column + 2 }
                        }
                        (GLSLExpression s)
                }
            )
            (ParserFast.Advanced.loop "" untilGlslEnd)
        )


untilGlslEnd : String -> Parser (ParserFast.Advanced.Step String String)
untilGlslEnd soFar =
    ParserFast.oneOf
        [ ParserFast.symbol "|]" (ParserFast.Advanced.Done soFar)
        , ParserFast.mapChompedString
            (\beforeVerticalBar () ->
                ParserFast.Advanced.Loop (soFar ++ beforeVerticalBar)
            )
            (ParserFast.chompIfFollowedBy (\c -> c /= '|')
                (ParserFast.chompWhile (\c -> c /= '|'))
            )
        , ParserFast.symbol "|" (ParserFast.Advanced.Loop (soFar ++ "|"))
        ]


listOrGlslExpression : Parser (WithComments (Node Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.mapWithStartAndEndPosition
            (\start elements end ->
                { comments = elements.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 1 }
                        , end = end
                        }
                        elements.syntax
                }
            )
            (ParserFast.map2
                (\commentsBefore elements ->
                    { comments = commentsBefore |> Rope.prependTo elements.comments
                    , syntax = elements.syntax
                    }
                )
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbol "]" { comments = Rope.empty, syntax = ListExpr [] })
                    (ParserFast.map4
                        (\head commentsAfterHead tail () ->
                            { comments =
                                head.comments
                                    |> Rope.prependTo commentsAfterHead
                                    |> Rope.prependTo tail.comments
                            , syntax = ListExpr (head.syntax :: tail.syntax)
                            }
                        )
                        expression
                        Layout.maybeLayout
                        (ParserWithComments.many
                            (ParserFast.symbolFollowedBy ","
                                (Layout.maybeAroundBothSides expression)
                            )
                        )
                        Tokens.squareEnd
                    )
                )
            )
        )



-- recordExpression


recordExpression : Parser (WithComments (Node Expression))
recordExpression =
    Parser.map
        (\() ->
            \commentsBefore ->
                \afterCurly ->
                    \( endRow, endColumn ) ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo afterCurly.comments
                        , end = { row = endRow, column = endColumn }
                        , expression = afterCurly.syntax
                        }
        )
        Tokens.curlyStart
        |= Layout.maybeLayout
        |= recordContentsCurlyEnd
        |= Parser.getPosition


expressionRecordEmptyWithComments : WithComments Expression
expressionRecordEmptyWithComments =
    { comments = Rope.empty, syntax = RecordExpr [] }


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    Parser.oneOf
        [ Parser.map
            (\( nameStartRow, nameStartColumn ) ->
                \name ->
                    \commentsAfterFunctionName ->
                        \afterNameBeforeFields ->
                            \tailFields ->
                                \commentsBeforeClosingCurly ->
                                    let
                                        nameNode : Node String
                                        nameNode =
                                            Node.singleLineStringFrom { row = nameStartRow, column = nameStartColumn }
                                                name
                                    in
                                    { comments =
                                        commentsAfterFunctionName
                                            |> Rope.prependTo afterNameBeforeFields.comments
                                            |> Rope.prependTo tailFields.comments
                                            |> Rope.prependTo commentsBeforeClosingCurly
                                    , syntax =
                                        case afterNameBeforeFields.syntax of
                                            RecordUpdateFirstSetter firstField ->
                                                RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                                            FieldsFirstValue firstFieldValue ->
                                                RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                                    }
            )
            Parser.getPosition
            |= Tokens.functionName
            |= Layout.maybeLayout
            |= Parser.oneOf
                [ Parser.map
                    (\() ->
                        \commentsBefore ->
                            \setterResult ->
                                { comments = commentsBefore |> Rope.prependTo setterResult.comments
                                , syntax = RecordUpdateFirstSetter setterResult.syntax
                                }
                    )
                    Tokens.pipe
                    |= Layout.maybeLayout
                    |= recordSetterNodeWithLayout
                , Parser.map
                    (\() ->
                        \commentsBefore ->
                            \expressionResult ->
                                \commentsAfter ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo expressionResult.comments
                                            |> Rope.prependTo commentsAfter
                                    , syntax = FieldsFirstValue expressionResult.syntax
                                    }
                    )
                    Tokens.equal
                    |= Layout.maybeLayout
                    |= expression
                    |= Layout.maybeLayout
                ]
            |= recordFields
            |= Layout.maybeLayoutUntilIgnored Parser.token "}"
        , Parser.map (\() -> expressionRecordEmptyWithComments) Tokens.curlyEnd
        ]


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        ((Parser.map
            (\() ->
                \commentsBefore ->
                    \setterResult ->
                        { comments = commentsBefore |> Rope.prependTo setterResult.comments
                        , syntax = setterResult.syntax
                        }
            )
            Tokens.comma
            |= Layout.maybeLayout
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
        |= Layout.maybeLayoutUntilIgnored Parser.token "="
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        |= Parser.getPosition


literalExpression : Parser { comments : Comments, end : Location, expression : Expression }
literalExpression =
    Parser.map
        (\string ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Literal string
                }
            )
            (Node.parserCore Tokens.functionName)
            (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy "=")
            Layout.maybeLayout
            expression
            -- This extra whitespace is just included for compatibility with earlier version
            -- TODO for v8: remove
            Layout.maybeLayout
        )


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start string end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } (Literal string)
            }
        )
        Tokens.singleOrTripleQuotedStringLiteral


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start char end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } (CharLiteral char)
            }
        )
        Tokens.characterLiteral



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    Parser.map
        (\() ->
            \commentsAfterBackslash ->
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
                                    { args = firstArg.syntax :: secondUpArgs.syntax
                                    , expression = expressionResult.syntax
                                    }
                                        |> LambdaExpression
                                }
        )
        Tokens.backSlash
        |= Layout.maybeLayout
        |= Patterns.pattern
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
                    Patterns.patternNotDirectlyComposing
                    Layout.maybeLayout
                )
            )
            expression
        )



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    Parser.map
        (\() ->
            \commentsAfterCase ->
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
                                        , cases = firstCase :: List.reverse lastToSecondCase
                                        }
                                }
        )
        Tokens.caseToken
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayoutUntilIgnored Parser.keyword "of"
        |= Layout.maybeLayout
        |= Parser.Extra.withIndent caseStatements


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
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
        |= Layout.maybeLayoutUntilIgnored Parser.token "->"
        |= Layout.maybeLayout
        |= expression
        |= ParserWithComments.manyWithoutReverse caseStatement


caseStatement : Parser (WithComments Case)
caseStatement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map4
            (\pattern commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    pattern.comments
                        |> Rope.prependTo commentsBeforeArrowRight
                        |> Rope.prependTo commentsAfterArrowRight
                        |> Rope.prependTo expr.comments
                , syntax = ( pattern.syntax, expr.syntax )
                }
            )
            Patterns.pattern
            (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy "->")
            Layout.maybeLayout
            expression
        )
        (Layout.optimisticLayout |> Parser.backtrackable)
        |. Layout.onTopIndentation ()
        |= Patterns.pattern
        |= Layout.maybeLayoutUntilIgnored Parser.token "->"
        |= Layout.maybeLayout
        |= expression



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    Parser.Extra.withIndent
        ((Parser.map
            (\() ->
                \commentsAfterLet ->
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
            Tokens.letToken
            |= Layout.maybeLayout
         )
            |= Parser.Extra.withIndent letDeclarationsIn
        )


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> Rope.prependTo commentsAfter
                        |> Rope.prependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
            (ParserWithComments.until Tokens.inToken blockElement)
        )


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentationFollowedBy
        (ParserFast.map2
            (\letDeclarationResult commentsAfter ->
                { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
                , syntax = letDeclarationResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunction
                letDestructuringDeclaration
            )
            Layout.optimisticLayout
        )


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    ParserFast.map4
        (\pattern commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (Node { start } _) =
                    pattern.syntax

                (Node { end } _) =
                    expressionResult.syntax
            in
            { comments =
                pattern.comments
                    |> Rope.prependTo commentsAfterPattern
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node { start = start, end = end }
                    (LetDestructuring pattern.syntax expressionResult.syntax)
            }
        )
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    ParserFast.map6
        (\((Node startNameRange startName) as startNameNode) commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
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

                startNameStart : Location
                startNameStart =
                    startNameRange.start
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            expressionResult.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (LetFunction
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = arguments.syntax
                                        , expression = expressionResult.syntax
                                        }
                                }
                            )
                    }
                        |> ParserFast.succeed

                Just signature ->
                    let
                        (Node implementationNameRange implementationName) =
                            signature.implementationName
                    in
                    if implementationName == startName ++ "" then
                        let
                            (Node expressionRange _) =
                                expressionResult.syntax
                        in
                        { comments = allComments
                        , syntax =
                            Node { start = startNameStart, end = expressionRange.end }
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
                            |> ParserFast.succeed

                    else
                        ParserFast.problem
                            ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        Parser.getPosition
        |= Tokens.functionName
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ Parser.map
                (\() ->
                    \commentsBeforeTypeAnnotation ->
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
                Tokens.colon
                |= Layout.maybeLayout
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
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


numberExpression : Parser (WithComments (Node Expression))
numberExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start n end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } n
            }
        )
        (Elm.Parser.Numbers.floatOrIntOrHex
            Floatable
            Integer
            Hex
        )


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    Parser.map
        (\() ->
            \commentsAfterIf ->
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
                                            , expression = IfBlock condition.syntax ifTrue.syntax ifFalse.syntax
                                            }
        )
        Tokens.ifToken
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayoutUntilIgnored Parser.keyword "then"
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayoutUntilIgnored Parser.keyword "else"
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
    Tokens.minus
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ Parser.chompIf (\next -> next == '\u{000D}' || next == '\n' || next == ' ')
                    |> Parser.Extra.continueWith (Parser.problem "negation sign cannot be followed by a space")
                , Parser.succeed ()
                ]
            )
        |> Parser.backtrackable


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
        (ParserFast.map8
            (\commentsAfterIf condition commentsBeforeThen commentsAfterThen ifTrue commentsBeforeElse commentsAfterElse ifFalse ->
                { comments =
                    commentsAfterIf
                        |> Rope.prependTo condition.comments
                        |> Rope.prependTo commentsBeforeThen
                        |> Rope.prependTo commentsAfterThen
                        |> Rope.prependTo ifTrue.comments
                        |> Rope.prependTo commentsBeforeElse
                        |> Rope.prependTo commentsAfterElse
                        |> Rope.prependTo ifFalse.comments
                , condition = condition.syntax
                , ifFalse = ifFalse.syntax
                , ifTrue = ifTrue.syntax
                }
            )
            (ParserFast.keywordFollowedBy "if" Layout.maybeLayout)
            expression
            (Layout.maybeLayoutUntilIgnored ParserFast.keywordFollowedBy "then")
            Layout.maybeLayout
            expression
            (Layout.maybeLayoutUntilIgnored ParserFast.keywordFollowedBy "else")
            Layout.maybeLayout
            expression
        )


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    ParserFast.mapWithStartPosition
        (\start subExpressionResult ->
            let
                (Node subExpressionRange _) =
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                Node { start = start, end = subExpressionRange.end }
                    (Negation subExpressionResult.syntax)
            }
        )
        (ParserFast.map2
            (\() subExpressionResult -> subExpressionResult)
            (ParserFast.symbol "-" () |> ParserFast.backtrackable)
            (extendedSubExpressionWithoutInitialLayout abovePrecedence95)
        )


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start reference end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } reference
            }
        )
        (ParserFast.map2
            (\firstName after ->
                case after of
                    Nothing ->
                        FunctionOrValue [] firstName

                    Just ( qualificationAfter, unqualified ) ->
                        FunctionOrValue (firstName :: qualificationAfter) unqualified
            )
            Tokens.typeName
            maybeDotReferenceExpressionTuple
        )


unqualifiedFunctionReferenceExpression : Parser (WithComments (Node Expression))
unqualifiedFunctionReferenceExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start unqualified end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (FunctionOrValue [] unqualified)
            }
        )
        Tokens.functionName


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2
                (ParserFast.map2
                    (\firstName after ->
                        Just
                            (case after of
                                Nothing ->
                                    ( [], firstName )

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
    Parser.map
        (\() ->
            \field ->
                \( endRow, endColumn ) ->
                    { comments = Rope.empty
                    , end = { row = endRow, column = endColumn }
                    , expression = RecordAccessFunction ("." ++ field)
                    }
        )
        Tokens.dot
        |= Tokens.functionName
        |= Parser.getPosition


tupledExpression : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpression =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                ((Parser.map
                    (\() ->
                        \( endRow, endColumn ) ->
                            { comments = Rope.empty
                            , end = { row = endRow, column = endColumn }
                            , expression = UnitExpr
                            }
                    )
                    Tokens.parensEnd
                    |= Parser.getPosition
                 )
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (Parser.map
                            (\() ->
                                \( endRow, endColumn ) ->
                                    { comments = Rope.empty
                                    , end = { row = endRow, column = endColumn }
                                    , expression = expressionPrefixOperatorMinus
                                    }
                            )
                            (Parser.symbol "-)")
                            |= Parser.getPosition
                       )
                    :: tupledExpressionInnerAfterOpeningParens
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )
        )
        Nothing


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    ParserFast.mapWithStartAndEndPosition
        (\start field end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (RecordAccessFunction ("." ++ field))
            }
        )
        (ParserFast.symbolFollowedBy "." Tokens.functionName)


tupledExpression : Parser (WithComments (Node Expression))
tupledExpression =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf
            (ParserFast.mapWithEndPosition
                (\() end ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            UnitExpr
                    }
                )
                (ParserFast.symbol ")" ())
                :: -- since `-` alone  could indicate negation or prefix operator,
                   -- we check for `-)` first
                   ParserFast.mapWithEndPosition
                    (\() end ->
                        { comments = Rope.empty
                        , syntax =
                            Node { start = { row = end.row, column = end.column - 3 }, end = end }
                                expressionPrefixOperatorMinus
                        }
                    )
                    (ParserFast.symbol "-)" ())
                :: tupledExpressionInnerAfterOpeningParens
                -- and since prefix operators are much more rare than e.g. parenthesized
                -- we check those later
                :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
            )
        )


expressionPrefixOperatorMinus : Expression
expressionPrefixOperatorMinus =
    PrefixOperator "-"


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser (WithComments (Node Expression)))
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                Parser.map
                    (\() ->
                        \( endRow, endColumn ) ->
                            { comments = Rope.empty
                            , end = { row = endRow, column = endColumn }
                            , expression = PrefixOperator allowedOperatorToken
                            }
                    )
                    (Parser.symbol (allowedOperatorToken ++ ")"))
                    |= Parser.getPosition
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
                                , expression = ParenthesizedExpression firstPart.syntax
                                }

                            _ ->
                                { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
                                , end = { row = endRow, column = endColumn }
                                , expression = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                                }
        )
        expression
        |= Layout.maybeLayout
        |= ParserWithComments.untilWithoutReverse Tokens.parensEnd
            (Parser.map
                (\() ->
                    \commentsBefore ->
                        \partResult ->
                            \commentsAfter ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo partResult.comments
                                        |> Rope.prependTo commentsAfter
                                , syntax = partResult.syntax
                                }
                )
                Tokens.comma
                |= Layout.maybeLayout
                |= expression
                |= Layout.maybeLayout
            )


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.mapWithStartAndEndPosition
        (\start tupled end ->
            { comments = tupled.comments
            , syntax =
                Node { start = { row = start.row, column = start.column - 1 }, end = end }
                    tupled.syntax
            }
        )
        (ParserFast.map4
            (\commentsBeforeFirstPart firstPart commentsAfterFirstPart tailPartsReverse ->
                case tailPartsReverse.syntax of
                    [] ->
                        { comments =
                            commentsBeforeFirstPart
                                |> Rope.prependTo firstPart.comments
                                |> Rope.prependTo commentsAfterFirstPart
                        , syntax = ParenthesizedExpression firstPart.syntax
                        }

                    _ ->
                        { comments =
                            commentsBeforeFirstPart
                                |> Rope.prependTo firstPart.comments
                                |> Rope.prependTo commentsAfterFirstPart
                                |> Rope.prependTo tailPartsReverse.comments
                        , syntax = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                        }
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserWithComments.untilWithoutReverse
                Tokens.parensEnd
                (ParserFast.map3
                    (\commentsBefore partResult commentsAfter ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo partResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = partResult.syntax
                        }
                    )
                    (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                    expression
                    Layout.maybeLayout
                )
            )
        )



---


subExpressionMap :
    Parser (WithComments ExtensionRight)
    -> Parser (WithComments (Node Expression))
subExpressionMap aboveCurrentPrecedenceLayout =
    let
        step :
            WithComments (Node Expression)
            -> Parser (Parser.Advanced.Step (WithComments (Node Expression)) a)
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
                                |> Parser.Advanced.Loop
                    )
                    aboveCurrentPrecedenceLayout
                    |= Layout.optimisticLayout
                , Parser.succeed
                    (Parser.Advanced.Done (toExtensionRightWith leftExpressionResult))
                ]
    in
    ParserFast.map3
        (\commentsBefore leftExpressionResult commentsAfter ->
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
            (\leftExpression -> Parser.Advanced.loop leftExpression step)


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight extensionRight ((Node { start } left) as leftNode) =
    case extensionRight of
        ExtendRightByRecordAccess ((Node { end } _) as field) ->
            Node { start = start, end = end }
                (Expression.RecordAccess leftNode field)

        ExtendRightByApplication ((Node { end } _) as right) ->
            Node { start = start, end = end }
                (Expression.Application
                    (case left of
                        Expression.Application (called :: firstArg :: secondArgUp) ->
                            called :: firstArg :: (secondArgUp ++ [ right ])

                        _ ->
                            [ leftNode, right ]
                    )
                )

        ExtendRightByOperation extendRightOperation ->
            let
                ((Node { end } _) as right) =
                    extendRightOperation.expression
            in
            Node { start = start, end = end }
                (OperatorApplication extendRightOperation.symbol extendRightOperation.direction leftNode right)


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
        |> ParserFast.oneOf


infixLeft : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixLeft precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (ParserFast.symbolFollowedBy symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Left, expression = right }
        )


infixNonAssociative : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociative precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (ParserFast.symbolFollowedBy symbol)
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
        (ParserFast.symbolFollowedBy symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Right, expression = right }
        )


lookBehindOneCharacterAndThen : (String -> Parser res) -> Parser res
lookBehindOneCharacterAndThen callback =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            callback (String.slice (offset - 1) offset source)
        )


infixLeftSubtraction : Int -> Parser (WithComments ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
infixLeftSubtraction precedence possibilitiesForPrecedence =
    infixHelp precedence
        possibilitiesForPrecedence
        (\next ->
            lookBehindOneCharacterAndThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Tokens.minusFollowedBySingleWhitespace next

                    else
                        ParserFast.symbolFollowedBy "-" next
                )
        )
        (\right ->
            ExtendRightByOperation { symbol = "-", direction = Infix.Left, expression = right }
        )


infixHelp :
    Int
    -> Parser (WithComments ExtensionRight)
    -> (Parser (WithComments (Node Expression)) -> Parser (WithComments (Node Expression)))
    -> (Node Expression -> ExtensionRight)
    -> ( Int, Parser (WithComments ExtensionRight) )
infixHelp leftPrecedence rightPrecedence operatorFollowedBy apply =
    ( leftPrecedence
    , ParserFast.map
        (\e ->
            { comments = e.comments
            , syntax = apply e.syntax
            }
        )
        (operatorFollowedBy
            (subExpressionMap rightPrecedence)
        )
    )


postfix : Int -> Parser (WithComments ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
postfix precedence operator =
    ( precedence
    , operator
    )


type ExtensionRight
    = ExtendRightByOperation { symbol : String, direction : Infix.InfixDirection, expression : Node Expression }
    | ExtendRightByApplication (Node Expression)
    | ExtendRightByRecordAccess (Node String)
