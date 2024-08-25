module Elm.Parser.Expression exposing (expression)

import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    ParserFast.oneOf14
        qualifiedOrVariantOrRecordConstructorReferenceExpression
        unqualifiedFunctionReferenceExpression
        literalExpression
        numberExpression
        tupledExpression
        listOrGlslExpression
        recordExpression
        caseExpression
        lambdaExpression
        letExpression
        ifBlockExpression
        recordAccessFunctionExpression
        negationOperation
        charLiteralExpression


extensionRightByPrecedence : List ( Int, Parser (WithComments ExtensionRight) )
extensionRightByPrecedence =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ recordAccessOptimisticLayout
    , infixLeft 1 (ParserFast.lazy (\() -> abovePrecedence1)) "|>"

    -- to avoid ambiguity between negated arguments and subtraction,
    -- calls must be checked AFTER subtraction
    , infixLeftSubtraction 6 (ParserFast.lazy (\() -> abovePrecedence6))
    , functionCall

    --
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (ParserFast.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (ParserFast.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "+"
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
    ]


expression : Parser (WithComments (Node Expression))
expression =
    extendedSubExpression abovePrecedence0


recordAccessOptimisticLayout : ( Int, Parser (WithComments ExtensionRight) )
recordAccessOptimisticLayout =
    postfix 100 recordAccessParserOptimisticLayout


recordAccessParserOptimisticLayout : Parser (WithComments ExtensionRight)
recordAccessParserOptimisticLayout =
    lookBehindOneCharacterAndThen
        (\c ->
            if c == " " || c == "\n" || c == "\u{000D}" then
                problemRecordAccessStartingWithSpace

            else
                dotFieldOptimisticLayout
        )


problemRecordAccessStartingWithSpace : ParserFast.Parser a
problemRecordAccessStartingWithSpace =
    ParserFast.problem "Record access can't start with a space"


dotFieldOptimisticLayout : ParserFast.Parser (WithComments ExtensionRight)
dotFieldOptimisticLayout =
    ParserFast.symbolFollowedBy "."
        (ParserFast.map2
            (\field commentsAfter ->
                { comments = commentsAfter
                , syntax =
                    ExtendRightByRecordAccess field
                }
            )
            Tokens.functionNameNode
            Layout.optimisticLayout
        )


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (ParserFast.lazy (\() -> abovePrecedence90))
        Layout.positivelyIndentedFollowedBy
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
            (ParserFast.loopUntil
                (ParserFast.symbol "|]" ())
                (ParserFast.oneOf2
                    (ParserFast.symbol "|" "|")
                    (ParserFast.while (\c -> c /= '|'))
                )
                ""
                (\extension soFar ->
                    soFar ++ extension ++ ""
                )
                identity
            )
        )


listOrGlslExpression : Parser (WithComments (Node Expression))
listOrGlslExpression =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithStartAndEndPosition
            (\start commentsBefore elements end ->
                { comments = commentsBefore |> Rope.prependTo elements.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 1 }
                        , end = end
                        }
                        elements.syntax
                }
            )
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = Rope.empty, syntax = ListExpr [] })
                (ParserFast.map3
                    (\head commentsAfterHead tail ->
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
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )



-- recordExpression


recordExpression : Parser (WithComments (Node Expression))
recordExpression =
    ParserFast.map2WithStartAndEndPosition
        (\start commentsBefore afterCurly end ->
            { comments =
                commentsBefore
                    |> Rope.prependTo afterCurly.comments
            , syntax = Node { start = start, end = end } afterCurly.syntax
            }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        recordContentsCurlyEnd


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    ParserFast.oneOf2
        (ParserFast.map5
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
            Tokens.functionNameNode
            Layout.maybeLayout
            (ParserFast.oneOf2
                (ParserFast.map2
                    (\commentsBefore setterResult ->
                        { comments = commentsBefore |> Rope.prependTo setterResult.comments
                        , syntax = RecordUpdateFirstSetter setterResult.syntax
                        }
                    )
                    (ParserFast.symbolFollowedBy "|" Layout.maybeLayout)
                    recordSetterNodeWithLayout
                )
                (ParserFast.map3
                    (\commentsBefore expressionResult commentsAfter ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo expressionResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = FieldsFirstValue expressionResult.syntax
                        }
                    )
                    (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
                    expression
                    Layout.maybeLayout
                )
            )
            recordFields
            (Layout.maybeLayoutUntilIgnored ParserFast.symbol "}")
        )
        (ParserFast.symbol "}" { comments = Rope.empty, syntax = RecordExpr [] })


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (ParserFast.map2
            (\commentsBefore setterResult ->
                { comments = commentsBefore |> Rope.prependTo setterResult.comments
                , syntax = setterResult.syntax
                }
            )
            (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
            recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    ParserFast.map5WithStartAndEndPosition
        (\start name commentsAfterFunctionName commentsAfterEquals expressionResult commentsAfterExpression end ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
                    |> Rope.prependTo commentsAfterExpression
            , syntax =
                Node { start = start, end = end } ( name, expressionResult.syntax )
            }
        )
        Tokens.functionNameNode
        (Layout.maybeLayoutUntilIgnored ParserFast.symbol "=")
        Layout.maybeLayout
        expression
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        Layout.maybeLayout


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
    ParserFast.map5WithStartPosition
        (\start commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs expressionResult ->
            let
                (Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                commentsAfterBackslash
                    |> Rope.prependTo firstArg.comments
                    |> Rope.prependTo commentsAfterFirstArg
                    |> Rope.prependTo secondUpArgs.comments
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node { start = start, end = expressionRange.end }
                    (LambdaExpression
                        { args = firstArg.syntax :: secondUpArgs.syntax
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.symbolFollowedBy "\\"
            Layout.maybeLayout
        )
        Patterns.patternNotDirectlyComposing
        Layout.maybeLayout
        (ParserWithComments.until
            (ParserFast.symbol "->" ())
            (ParserFast.map2
                (\patternResult commentsAfter ->
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



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    ParserFast.map5WithStartPosition
        (\start commentsAfterCase casedExpressionResult commentsBeforeOf commentsAfterOf casesResult ->
            let
                ( firstCase, lastToSecondCase ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> Rope.prependTo casedExpressionResult.comments
                    |> Rope.prependTo commentsBeforeOf
                    |> Rope.prependTo commentsAfterOf
                    |> Rope.prependTo casesResult.comments
            , syntax =
                Node
                    { start = start
                    , end =
                        case lastToSecondCase of
                            ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                lastCaseExpressionRange.end

                            [] ->
                                let
                                    ( _, Node firstCaseExpressionRange _ ) =
                                        firstCase
                                in
                                firstCaseExpressionRange.end
                    }
                    (CaseExpression
                        { expression = casedExpressionResult.syntax
                        , cases = firstCase :: List.reverse lastToSecondCase
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "case" Layout.maybeLayout)
        expression
        (Layout.maybeLayoutUntilIgnored ParserFast.keyword "of")
        Layout.maybeLayout
        (ParserFast.withIndentSetToColumn caseStatements)


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
        (Layout.maybeLayoutUntilIgnored ParserFast.symbol "->")
        Layout.maybeLayout
        expression
        (ParserWithComments.manyWithoutReverse caseStatement)


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
            (Layout.maybeLayoutUntilIgnored ParserFast.symbol "->")
            Layout.maybeLayout
            expression
        )



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    ParserFast.map3
        (\declarations commentsAfterIn expressionResult ->
            let
                (Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                declarations.comments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo expressionResult.comments
            , syntax =
                Node { start = declarations.start, end = expressionRange.end }
                    (LetExpression
                        { declarations = declarations.declarations
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.map2WithStartPosition
                (\start commentsAfterLet declarations ->
                    { comments =
                        commentsAfterLet
                            |> Rope.prependTo declarations.comments
                    , declarations = declarations.syntax
                    , start = start
                    }
                )
                (ParserFast.keywordFollowedBy "let" Layout.maybeLayout)
                (ParserFast.withIndentSetToColumn letDeclarationsIn)
            )
        )
        -- check that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        (Layout.positivelyIndentedPlusFollowedBy 2
            Layout.maybeLayout
        )
        expression


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
    ParserFast.map6WithStartPosition
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
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

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

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
        )
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        expression
        |> ParserFast.validate
            (\result ->
                let
                    (Node _ letDeclaration) =
                        result.syntax
                in
                case letDeclaration of
                    LetDestructuring _ _ ->
                        True

                    LetFunction letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""
            )
            "Expected to find the same name for declaration and signature"


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
        (ParserFast.floatOrIntOrHex
            Floatable
            Integer
            Hex
        )


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    ParserFast.map8WithStartPosition
        (\start commentsAfterIf condition commentsBeforeThen commentsAfterThen ifTrue commentsBeforeElse commentsAfterElse ifFalse ->
            let
                (Node ifFalseRange _) =
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
            , syntax =
                Node { start = start, end = ifFalseRange.end }
                    (IfBlock
                        condition.syntax
                        ifTrue.syntax
                        ifFalse.syntax
                    )
            }
        )
        (ParserFast.keywordFollowedBy "if" Layout.maybeLayout)
        expression
        (Layout.maybeLayoutUntilIgnored ParserFast.keyword "then")
        Layout.maybeLayout
        expression
        (Layout.maybeLayoutUntilIgnored ParserFast.keyword "else")
        Layout.maybeLayout
        expression


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    ParserFast.map2WithStartPosition
        (\start () subExpressionResult ->
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
        (ParserFast.symbolBacktrackable "-" ())
        (extendedSubExpressionWithoutInitialLayout abovePrecedence95)


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    ParserFast.map2WithStartAndEndPosition
        (\start firstName after end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (case after of
                        Nothing ->
                            FunctionOrValue [] firstName

                        Just ( qualificationAfter, unqualified ) ->
                            FunctionOrValue (firstName :: qualificationAfter) unqualified
                    )
            }
        )
        Tokens.typeName
        maybeDotReferenceExpressionTuple


unqualifiedFunctionReferenceExpression : Parser (WithComments (Node Expression))
unqualifiedFunctionReferenceExpression =
    Tokens.functionNameMapWithRange
        (\range unqualified ->
            { comments = Rope.empty
            , syntax =
                Node range (FunctionOrValue [] unqualified)
            }
        )


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                ( firstName :: qualificationAfter, unqualified )
                    )
                    Tokens.typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\unqualified -> Just ( [], unqualified ))
                Tokens.functionName
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
        (ParserFast.oneOf4
            (ParserFast.symbolWithEndPosition ")"
                (\end ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            UnitExpr
                    }
                )
            )
            -- since `-` alone  could indicate negation or prefix operator,
            -- we check for `-)` first
            (ParserFast.symbolWithEndPosition "-)"
                (\end ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = { row = end.row, column = end.column - 3 }, end = end }
                            expressionPrefixOperatorMinus
                    }
                )
            )
            tupledExpressionInnerAfterOpeningParens
            -- and since prefix operators are much more rare than e.g. parenthesized
            -- we check those later
            (ParserFast.oneOf allowedPrefixOperatorExceptMinusThenClosingParensOneOf)
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
                ParserFast.symbolWithEndPosition
                    (allowedOperatorToken ++ ")")
                    (\end ->
                        { comments = Rope.empty
                        , syntax =
                            Node { start = { row = end.row, column = end.column - prefixOperatorLength }, end = end }
                                (PrefixOperator allowedOperatorToken)
                        }
                    )
            )


tupledExpressionInnerAfterOpeningParens : Parser (WithComments (Node Expression))
tupledExpressionInnerAfterOpeningParens =
    ParserFast.map4WithStartAndEndPosition
        (\start commentsBeforeFirstPart firstPart commentsAfterFirstPart tailPartsReverse end ->
            case tailPartsReverse.syntax of
                [] ->
                    { comments =
                        commentsBeforeFirstPart
                            |> Rope.prependTo firstPart.comments
                            |> Rope.prependTo commentsAfterFirstPart
                    , syntax =
                        Node { start = { row = start.row, column = start.column - 1 }, end = end }
                            (ParenthesizedExpression firstPart.syntax)
                    }

                _ ->
                    { comments =
                        commentsBeforeFirstPart
                            |> Rope.prependTo firstPart.comments
                            |> Rope.prependTo commentsAfterFirstPart
                            |> Rope.prependTo tailPartsReverse.comments
                    , syntax =
                        Node { start = { row = start.row, column = start.column - 1 }, end = end }
                            (TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax))
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



---


extendedSubExpression :
    Parser (WithComments ExtensionRight)
    -> Parser (WithComments (Node Expression))
extendedSubExpression aboveCurrentPrecedenceLayout =
    ParserFast.map4
        (\commentsBefore leftExpressionResult commentsAfter extensionsRight ->
            { comments =
                commentsBefore
                    |> Rope.prependTo leftExpressionResult.comments
                    |> Rope.prependTo commentsAfter
                    |> Rope.prependTo extensionsRight.comments
            , syntax =
                leftExpressionResult.syntax
                    |> applyExtensionsRightReverse extensionsRight.syntax
            }
        )
        Layout.optimisticLayout
        (ParserFast.lazy (\() -> subExpression))
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            aboveCurrentPrecedenceLayout
        )


applyExtensionsRightReverse : List ExtensionRight -> Node Expression -> Node Expression
applyExtensionsRightReverse extensionsRight leftExpression =
    List.foldr
        (\extensionRight soFar ->
            soFar |> applyExtensionRight extensionRight
        )
        leftExpression
        extensionsRight


extendedSubExpressionWithoutInitialLayout :
    Parser (WithComments ExtensionRight)
    -> Parser (WithComments (Node Expression))
extendedSubExpressionWithoutInitialLayout aboveCurrentPrecedenceLayout =
    ParserFast.map3
        (\leftExpressionResult commentsAfter extensionsRight ->
            { comments =
                leftExpressionResult.comments
                    |> Rope.prependTo commentsAfter
                    |> Rope.prependTo extensionsRight.comments
            , syntax =
                leftExpressionResult.syntax
                    |> applyExtensionsRightReverse extensionsRight.syntax
            }
        )
        (ParserFast.lazy (\() -> subExpression))
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            aboveCurrentPrecedenceLayout
        )


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
    extensionRightByPrecedence
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
    let
        subtractionWithWhitespaceAfterMinus : Parser (WithComments ExtensionRight)
        subtractionWithWhitespaceAfterMinus =
            ParserFast.map
                (\e ->
                    { comments = e.comments
                    , syntax = ExtendRightByOperation { symbol = "-", direction = Infix.Left, expression = e.syntax }
                    }
                )
                (ParserFast.chompIfWhitespaceFollowedBy
                    (extendedSubExpression possibilitiesForPrecedence)
                )

        subtractionWithoutWhitespace : Parser (WithComments ExtensionRight)
        subtractionWithoutWhitespace =
            ParserFast.map
                (\e ->
                    { comments = e.comments
                    , syntax = ExtendRightByOperation { symbol = "-", direction = Infix.Left, expression = e.syntax }
                    }
                )
                (extendedSubExpression possibilitiesForPrecedence)
    in
    ( precedence
    , ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        subtractionWithWhitespaceAfterMinus

                    "\n" ->
                        subtractionWithWhitespaceAfterMinus

                    "\u{000D}" ->
                        subtractionWithWhitespaceAfterMinus

                    _ ->
                        subtractionWithoutWhitespace
            )
        )
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
            (extendedSubExpression rightPrecedence)
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
