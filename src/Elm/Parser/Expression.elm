module Elm.Parser.Expression exposing (expression)

import CustomParser exposing (Parser)
import CustomParser.Advanced
import CustomParser.Extra
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
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    CustomParser.oneOf
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
    , infixLeft 1 (CustomParser.lazy (\() -> abovePrecedence1)) "|>"
    , infixRight 5 (CustomParser.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (CustomParser.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (CustomParser.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (CustomParser.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (CustomParser.lazy (\() -> abovePrecedence6)) "+"
    , infixLeftSubtraction 6 (CustomParser.lazy (\() -> abovePrecedence6))
    , infixLeft 6 (CustomParser.lazy (\() -> abovePrecedence6)) "|."
    , infixRight 3 (CustomParser.lazy (\() -> abovePrecedence2)) "&&"
    , infixLeft 5 (CustomParser.lazy (\() -> abovePrecedence5)) "|="
    , infixLeft 9 (CustomParser.lazy (\() -> abovePrecedence9)) "<<"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "/="
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "//"
    , infixLeft 7 (CustomParser.lazy (\() -> abovePrecedence7)) "/"
    , infixRight 7 (CustomParser.lazy (\() -> abovePrecedence6)) "</>"
    , infixRight 2 (CustomParser.lazy (\() -> abovePrecedence1)) "||"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "<="
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) ">="
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) ">"
    , infixLeft 8 (CustomParser.lazy (\() -> abovePrecedence8)) "<?>"
    , infixNonAssociative 4 (CustomParser.lazy (\() -> abovePrecedence4)) "<"
    , infixRight 8 (CustomParser.lazy (\() -> abovePrecedence7)) "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser (WithComments (Node Expression))
expression =
    subExpressionMap abovePrecedence0


recordAccess : ( Int, Parser (WithComments ExtensionRight) )
recordAccess =
    postfix 100 recordAccessParser


recordAccessParser : Parser (WithComments ExtensionRight)
recordAccessParser =
    lookBehindOneCharacterAndThen
        (\c ->
            if c == " " || c == "\n" || c == "\u{000D}" then
                problemRecordAccessStartingWithSpace

            else
                dotField
        )


problemRecordAccessStartingWithSpace : CustomParser.Parser a
problemRecordAccessStartingWithSpace =
    CustomParser.problem "Record access can't start with a space"


dotField : CustomParser.Parser (WithComments ExtensionRight)
dotField =
    CustomParser.symbolFollowedBy "."
        (CustomParser.mapWithStartAndEndPosition
            (\nameStart name nameEnd ->
                { comments = Rope.empty
                , syntax =
                    ExtendRightByRecordAccess
                        (Node { start = nameStart, end = nameEnd } name)
                }
            )
            Tokens.functionName
        )


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (CustomParser.lazy (\() -> abovePrecedence90))
        Layout.positivelyIndentedFollowedBy
        ExtendRightByApplication


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    CustomParser.symbolFollowedBy "glsl|"
        (CustomParser.mapWithStartAndEndPosition
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
            (CustomParser.Advanced.loop "" untilGlslEnd)
        )


untilGlslEnd : String -> Parser (CustomParser.Advanced.Step String String)
untilGlslEnd soFar =
    CustomParser.oneOf
        [ CustomParser.symbol "|]" (CustomParser.Advanced.Done soFar)
        , CustomParser.mapChompedString
            (\beforeVerticalBar () ->
                CustomParser.Advanced.Loop (soFar ++ beforeVerticalBar)
            )
            (CustomParser.chompIf (\c -> c /= '|')
                |> CustomParser.ignore
                    (CustomParser.chompWhile (\c -> c /= '|'))
            )
        , CustomParser.symbol "|" (CustomParser.Advanced.Loop (soFar ++ "|"))
        ]


listOrGlslExpression : Parser (WithComments (Node Expression))
listOrGlslExpression =
    CustomParser.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
expressionAfterOpeningSquareBracket =
    CustomParser.oneOf
        [ glslExpressionAfterOpeningSquareBracket
        , CustomParser.mapWithStartAndEndPosition
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
            (CustomParser.map2
                (\commentsBefore elements ->
                    { comments = commentsBefore |> Rope.prependTo elements.comments
                    , syntax = elements.syntax
                    }
                )
                Layout.maybeLayout
                (CustomParser.oneOf
                    [ CustomParser.symbol "]" { comments = Rope.empty, syntax = ListExpr [] }
                    , CustomParser.map4
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
                            (CustomParser.symbolFollowedBy ","
                                (Layout.maybeAroundBothSides expression)
                            )
                        )
                        Tokens.squareEnd
                    ]
                )
            )
        ]



-- recordExpression


recordExpression : Parser (WithComments (Node Expression))
recordExpression =
    CustomParser.map2
        (\commentsBefore afterCurly ->
            { comments =
                commentsBefore
                    |> Rope.prependTo afterCurly.comments
            , syntax = afterCurly.syntax
            }
        )
        (CustomParser.symbolFollowedBy "{" Layout.maybeLayout)
        recordContentsCurlyEnd
        |> Node.parser


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    CustomParser.oneOf
        [ CustomParser.map5
            (\nameNode commentsAfterFunctionName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
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
            (Node.parserCore Tokens.functionName)
            Layout.maybeLayout
            (CustomParser.oneOf
                [ CustomParser.map2
                    (\commentsBefore setterResult ->
                        { comments = commentsBefore |> Rope.prependTo setterResult.comments
                        , syntax = RecordUpdateFirstSetter setterResult.syntax
                        }
                    )
                    (CustomParser.symbolFollowedBy "|" Layout.maybeLayout)
                    recordSetterNodeWithLayout
                , CustomParser.map3
                    (\commentsBefore expressionResult commentsAfter ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo expressionResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = FieldsFirstValue expressionResult.syntax
                        }
                    )
                    (CustomParser.symbolFollowedBy "=" Layout.maybeLayout)
                    expression
                    Layout.maybeLayout
                ]
            )
            recordFields
            (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "}")
        , CustomParser.symbol "}" { comments = Rope.empty, syntax = RecordExpr [] }
        ]


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (CustomParser.map2
            (\commentsBefore setterResult ->
                { comments = commentsBefore |> Rope.prependTo setterResult.comments
                , syntax = setterResult.syntax
                }
            )
            (CustomParser.symbolFollowedBy "," Layout.maybeLayout)
            recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    Node.parser
        (CustomParser.map5
            (\name commentsAfterFunctionName commentsAfterEquals expressionResult commentsAfterExpression ->
                { comments =
                    commentsAfterFunctionName
                        |> Rope.prependTo commentsAfterEquals
                        |> Rope.prependTo expressionResult.comments
                        |> Rope.prependTo commentsAfterExpression
                , syntax =
                    ( name, expressionResult.syntax )
                }
            )
            (Node.parserCore Tokens.functionName)
            (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "=")
            Layout.maybeLayout
            expression
            -- This extra whitespace is just included for compatibility with earlier version
            -- TODO for v8: remove
            Layout.maybeLayout
        )


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    CustomParser.mapWithStartAndEndPosition
        (\start string end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } (Literal string)
            }
        )
        Tokens.singleOrTripleQuotedStringLiteral


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    CustomParser.mapWithStartAndEndPosition
        (\start char end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } (CharLiteral char)
            }
        )
        Tokens.characterLiteral



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    CustomParser.mapWithStartPosition
        (\start lambda ->
            let
                (Node expressionRange _) =
                    lambda.expression
            in
            { comments = lambda.comments
            , syntax =
                Node { start = start, end = expressionRange.end }
                    (LambdaExpression
                        { args = lambda.args
                        , expression = lambda.expression
                        }
                    )
            }
        )
        (CustomParser.map5
            (\commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs expressionResult ->
                { args = firstArg.syntax :: secondUpArgs.syntax
                , comments =
                    commentsAfterBackslash
                        |> Rope.prependTo firstArg.comments
                        |> Rope.prependTo commentsAfterFirstArg
                        |> Rope.prependTo secondUpArgs.comments
                        |> Rope.prependTo expressionResult.comments
                , expression = expressionResult.syntax
                }
            )
            (CustomParser.symbolFollowedBy "\\" Layout.maybeLayout)
            Patterns.pattern
            Layout.maybeLayout
            (ParserWithComments.until
                Tokens.arrowRight
                (CustomParser.map2
                    (\patternResult commentsAfter ->
                        { comments =
                            patternResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = patternResult.syntax
                        }
                    )
                    Patterns.pattern
                    Layout.maybeLayout
                )
            )
            expression
        )



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    CustomParser.mapWithStartPosition
        (\start caseBlock ->
            { comments = caseBlock.comments
            , syntax =
                Node
                    { start = start
                    , end =
                        case caseBlock.lastToSecondCase of
                            ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                lastCaseExpressionRange.end

                            [] ->
                                let
                                    ( _, Node firstCaseExpressionRange _ ) =
                                        caseBlock.firstCase
                                in
                                firstCaseExpressionRange.end
                    }
                    (CaseExpression
                        { expression = caseBlock.casedExpression
                        , cases = caseBlock.firstCase :: List.reverse caseBlock.lastToSecondCase
                        }
                    )
            }
        )
        (CustomParser.map5
            (\commentsAfterCase casedExpressionResult commentsBeforeOf commentsAfterOf casesResult ->
                let
                    ( firstCase, lastToSecondCase ) =
                        casesResult.syntax
                in
                { casedExpression = casedExpressionResult.syntax
                , comments =
                    commentsAfterCase
                        |> Rope.prependTo casedExpressionResult.comments
                        |> Rope.prependTo commentsBeforeOf
                        |> Rope.prependTo commentsAfterOf
                        |> Rope.prependTo casesResult.comments
                , firstCase = firstCase
                , lastToSecondCase = lastToSecondCase
                }
            )
            (CustomParser.keywordFollowedBy "case" Layout.maybeLayout)
            expression
            (Layout.maybeLayoutUntilIgnored CustomParser.keywordFollowedBy "of")
            Layout.maybeLayout
            (CustomParser.Extra.withIndent caseStatements)
        )


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    CustomParser.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> Rope.prependTo commentsAfterFirstCasePattern
                    |> Rope.prependTo commentsAfterFirstCaseArrowRight
                    |> Rope.prependTo lastToSecondCase.comments
                    |> Rope.prependTo firstCaseExpressionResult.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        Patterns.pattern
        (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "->")
        Layout.maybeLayout
        expression
        (ParserWithComments.manyWithoutReverse caseStatement)


caseStatement : Parser (WithComments Case)
caseStatement =
    CustomParser.map6
        (\commentsBeforeCase () pattern commentsBeforeArrowRight commentsAfterArrowRight expr ->
            { comments =
                commentsBeforeCase
                    |> Rope.prependTo pattern.comments
                    |> Rope.prependTo commentsBeforeArrowRight
                    |> Rope.prependTo commentsAfterArrowRight
                    |> Rope.prependTo expr.comments
            , syntax = ( pattern.syntax, expr.syntax )
            }
        )
        (Layout.optimisticLayout |> CustomParser.backtrackable)
        Layout.onTopIndentation
        Patterns.pattern
        (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "->")
        Layout.maybeLayout
        expression



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    CustomParser.mapWithStartPosition
        (\start letBlock ->
            let
                (Node expressionRange _) =
                    letBlock.expression
            in
            { comments = letBlock.comments
            , syntax =
                Node { start = start, end = expressionRange.end }
                    (LetExpression
                        { declarations = letBlock.declarations
                        , expression = letBlock.expression
                        }
                    )
            }
        )
        (CustomParser.map4
            (\declarations () commentsAfterIn expressionResult ->
                { comments =
                    declarations.comments
                        |> Rope.prependTo commentsAfterIn
                        |> Rope.prependTo expressionResult.comments
                , declarations = declarations.syntax
                , expression = expressionResult.syntax
                }
            )
            (CustomParser.Extra.withIndent
                (CustomParser.map2
                    (\commentsAfterLet declarations ->
                        { comments =
                            commentsAfterLet
                                |> Rope.prependTo declarations.comments
                        , syntax = declarations.syntax
                        }
                    )
                    (CustomParser.keywordFollowedBy "let" Layout.maybeLayout)
                    (CustomParser.Extra.withIndent letDeclarationsIn)
                )
            )
            -- check that the `in` token used as the end parser in letDeclarationsIn is indented correctly
            (Layout.positivelyIndentedPlus 2)
            Layout.maybeLayout
            expression
        )


letDeclarationsIn : Parser (WithComments (List (Node LetDeclaration)))
letDeclarationsIn =
    CustomParser.map4
        (\() headLetResult commentsAfter tailLetResult ->
            { comments =
                headLetResult.comments
                    |> Rope.prependTo commentsAfter
                    |> Rope.prependTo tailLetResult.comments
            , syntax = headLetResult.syntax :: tailLetResult.syntax
            }
        )
        Layout.onTopIndentation
        (CustomParser.oneOf
            [ letFunction
            , letDestructuringDeclaration
            ]
        )
        Layout.optimisticLayout
        (ParserWithComments.until Tokens.inToken blockElement)


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    CustomParser.map3
        (\() letDeclarationResult commentsAfter ->
            { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
            , syntax = letDeclarationResult.syntax
            }
        )
        Layout.onTopIndentation
        (CustomParser.oneOf
            [ letFunction
            , letDestructuringDeclaration
            ]
        )
        Layout.optimisticLayout


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    CustomParser.map2
        (\pattern expressionResult ->
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
        Patterns.pattern
        (CustomParser.symbolFollowedBy "=" expression)


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    CustomParser.map6
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
                        |> CustomParser.succeed

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
                            |> CustomParser.succeed

                    else
                        CustomParser.problem
                            ("Expected to find the declaration for " ++ startName ++ " but found " ++ implementationName)
        )
        (Node.parserCore Tokens.functionName)
        Layout.maybeLayout
        (CustomParser.oneOf
            [ CustomParser.map5
                (\commentsBeforeTypeAnnotation typeAnnotationResult commentsAfterTypeAnnotation implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> Rope.prependTo typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfterTypeAnnotation
                                |> Rope.prependTo afterImplementationName
                        , implementationName = implementationName
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                (CustomParser.symbolFollowedBy ":" Layout.maybeLayout)
                TypeAnnotation.typeAnnotation
                Layout.layoutStrict
                (Node.parserCore Tokens.functionName)
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


numberExpression : Parser (WithComments (Node Expression))
numberExpression =
    CustomParser.mapWithStartAndEndPosition
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
    CustomParser.mapWithStartPosition
        (\start ifBlock ->
            let
                (Node ifFalseRange _) =
                    ifBlock.ifFalse
            in
            { comments = ifBlock.comments
            , syntax =
                Node { start = start, end = ifFalseRange.end }
                    (IfBlock
                        ifBlock.condition
                        ifBlock.ifTrue
                        ifBlock.ifFalse
                    )
            }
        )
        (CustomParser.map8
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
            (CustomParser.keywordFollowedBy "if" Layout.maybeLayout)
            expression
            (Layout.maybeLayoutUntilIgnored CustomParser.keywordFollowedBy "then")
            Layout.maybeLayout
            expression
            (Layout.maybeLayoutUntilIgnored CustomParser.keywordFollowedBy "else")
            Layout.maybeLayout
            expression
        )


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    CustomParser.mapWithStartPosition
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
        (CustomParser.map2
            (\() subExpressionResult -> subExpressionResult)
            (CustomParser.symbol "-" () |> CustomParser.backtrackable)
            (extendedSubExpressionWithoutInitialLayout abovePrecedence95)
        )


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    CustomParser.mapWithStartAndEndPosition
        (\start reference end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } reference
            }
        )
        (CustomParser.map2
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
    CustomParser.mapWithStartAndEndPosition
        (\start unqualified end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (FunctionOrValue [] unqualified)
            }
        )
        Tokens.functionName


maybeDotReferenceExpressionTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    CustomParser.oneOf
        [ CustomParser.symbolFollowedBy "."
            (CustomParser.oneOf
                [ CustomParser.map2
                    (\firstName after ->
                        Just
                            (case after of
                                Nothing ->
                                    ( [], firstName )

                                Just ( qualificationAfter, unqualified ) ->
                                    ( firstName :: qualificationAfter, unqualified )
                            )
                    )
                    Tokens.typeName
                    (CustomParser.lazy (\() -> maybeDotReferenceExpressionTuple))
                , CustomParser.map (\unqualified -> Just ( [], unqualified )) Tokens.functionName
                ]
            )
        , CustomParser.succeed Nothing
        ]


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    CustomParser.mapWithStartAndEndPosition
        (\start field end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (RecordAccessFunction ("." ++ field))
            }
        )
        (CustomParser.symbolFollowedBy "." Tokens.functionName)


tupledExpression : Parser (WithComments (Node Expression))
tupledExpression =
    CustomParser.symbolFollowedBy "("
        (CustomParser.oneOf
            (CustomParser.mapWithEndPosition
                (\() end ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            UnitExpr
                    }
                )
                (CustomParser.symbol ")" ())
                :: -- since `-` alone  could indicate negation or prefix operator,
                   -- we check for `-)` first
                   CustomParser.mapWithEndPosition
                    (\() end ->
                        { comments = Rope.empty
                        , syntax =
                            Node { start = { row = end.row, column = end.column - 3 }, end = end }
                                expressionPrefixOperatorMinus
                        }
                    )
                    (CustomParser.symbol "-)" ())
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
                let
                    prefixOperatorLength : Int
                    prefixOperatorLength =
                        2 + String.length allowedOperatorToken
                in
                CustomParser.mapWithEndPosition
                    (\() end ->
                        { comments = Rope.empty
                        , syntax =
                            Node { start = { row = end.row, column = end.column - prefixOperatorLength }, end = end }
                                (PrefixOperator allowedOperatorToken)
                        }
                    )
                    (CustomParser.symbol (allowedOperatorToken ++ ")") ())
            )


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    CustomParser.mapWithStartAndEndPosition
        (\start tupled end ->
            { comments = tupled.comments
            , syntax =
                Node { start = { row = start.row, column = start.column - 1 }, end = end }
                    tupled.syntax
            }
        )
        (CustomParser.map3
            (\firstPart commentsAfterFirstPart tailPartsReverse ->
                case tailPartsReverse.syntax of
                    [] ->
                        { comments = firstPart.comments |> Rope.prependTo commentsAfterFirstPart
                        , syntax = ParenthesizedExpression firstPart.syntax
                        }

                    _ ->
                        { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
                        , syntax = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                        }
            )
            expression
            Layout.maybeLayout
            (ParserWithComments.untilWithoutReverse
                Tokens.parensEnd
                (CustomParser.map3
                    (\commentsBefore partResult commentsAfter ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo partResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = partResult.syntax
                        }
                    )
                    (CustomParser.symbolFollowedBy "," Layout.maybeLayout)
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
            ->
                Parser
                    (CustomParser.Advanced.Step
                        (WithComments (Node Expression))
                        (WithComments (Node Expression))
                    )
        step leftExpressionResult =
            subExpressionLoopStep aboveCurrentPrecedenceLayout leftExpressionResult
    in
    CustomParser.map3
        (\commentsBefore leftExpressionResult commentsAfter ->
            { comments =
                commentsBefore
                    |> Rope.prependTo leftExpressionResult.comments
                    |> Rope.prependTo commentsAfter
            , syntax = leftExpressionResult.syntax
            }
        )
        Layout.optimisticLayout
        (CustomParser.lazy (\() -> subExpression))
        Layout.optimisticLayout
        |> CustomParser.andThen
            (\leftExpression -> CustomParser.Advanced.loop leftExpression step)


extendedSubExpressionWithoutInitialLayout :
    Parser (WithComments ExtensionRight)
    -> Parser (WithComments (Node Expression))
extendedSubExpressionWithoutInitialLayout aboveCurrentPrecedenceLayout =
    let
        step :
            WithComments (Node Expression)
            ->
                Parser
                    (CustomParser.Advanced.Step
                        (WithComments (Node Expression))
                        (WithComments (Node Expression))
                    )
        step leftExpressionResult =
            subExpressionLoopStep aboveCurrentPrecedenceLayout leftExpressionResult
    in
    CustomParser.map2
        (\leftExpressionResult commentsAfter ->
            { comments =
                leftExpressionResult.comments
                    |> Rope.prependTo commentsAfter
            , syntax = leftExpressionResult.syntax
            }
        )
        (CustomParser.lazy (\() -> subExpression))
        Layout.optimisticLayout
        |> CustomParser.andThen
            (\leftExpression -> CustomParser.Advanced.loop leftExpression step)


subExpressionLoopStep :
    Parser (WithComments ExtensionRight)
    -> WithComments (Node Expression)
    ->
        Parser
            (CustomParser.Advanced.Step
                (WithComments (Node Expression))
                (WithComments (Node Expression))
            )
subExpressionLoopStep aboveCurrentPrecedenceLayout leftExpressionResult =
    CustomParser.oneOf
        [ CustomParser.map2
            (\extensionRight commentsAfter ->
                { comments =
                    leftExpressionResult.comments
                        |> Rope.prependTo extensionRight.comments
                        |> Rope.prependTo commentsAfter
                , syntax =
                    leftExpressionResult.syntax
                        |> applyExtensionRight extensionRight.syntax
                }
                    |> CustomParser.Advanced.Loop
            )
            aboveCurrentPrecedenceLayout
            Layout.optimisticLayout
        , CustomParser.succeed
            (CustomParser.Advanced.Done leftExpressionResult)
        ]


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
        |> CustomParser.oneOf


infixLeft : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixLeft precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (CustomParser.symbolFollowedBy symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Left, expression = right }
        )


infixNonAssociative : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociative precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (CustomParser.symbolFollowedBy symbol)
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
        (CustomParser.symbolFollowedBy symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Right, expression = right }
        )


lookBehindOneCharacterAndThen : (String -> Parser res) -> Parser res
lookBehindOneCharacterAndThen callback =
    CustomParser.offsetSourceAndThen
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
                        CustomParser.symbolFollowedBy "-" next
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
    , CustomParser.map
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
