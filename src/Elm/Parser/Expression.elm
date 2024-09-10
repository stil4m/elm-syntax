module Elm.Parser.Expression exposing (expression)

import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpression : Parser (WithComments (Node Expression))
subExpression =
    ParserFast.oneOf14
        qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess
        unqualifiedFunctionReferenceExpressionFollowedByRecordAccess
        literalExpression
        numberExpression
        tupledExpressionIfNecessaryFollowedByRecordAccess
        listOrGlslExpression
        recordExpressionFollowedByRecordAccess
        caseExpression
        lambdaExpression
        letExpression
        ifBlockExpression
        recordAccessFunctionExpression
        negationOperation
        charLiteralExpression


multiRecordAccess : ParserFast.Parser (List (Node String))
multiRecordAccess =
    ParserFast.loopWhileSucceeds
        (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
        []
        (::)
        List.reverse


multiRecordAccessMap : (List (Node String) -> res) -> ParserFast.Parser res
multiRecordAccessMap fieldsToRes =
    ParserFast.loopWhileSucceeds
        (ParserFast.symbolFollowedBy "." Tokens.functionNameNode)
        []
        (::)
        (\reversed -> fieldsToRes (List.reverse reversed))


extensionRightByPrecedence : List ( Int, Parser (WithComments ExtensionRight) )
extensionRightByPrecedence =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ infixLeft 1 (ParserFast.lazy (\() -> abovePrecedence1)) "|>"
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "++"
    , infixRight 1 (ParserFast.lazy (\() -> abovePrecedence0)) "<|"
    , infixRight 9 (ParserFast.lazy (\() -> abovePrecedence8)) ">>"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "=="
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "*"
    , infixRight 5 (ParserFast.lazy (\() -> abovePrecedence4)) "::"
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "+"
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "-"
    , infixLeft 6 (ParserFast.lazy (\() -> abovePrecedence6)) "|."
    , infixRight 3 (ParserFast.lazy (\() -> abovePrecedence2)) "&&"
    , infixLeft 5 (ParserFast.lazy (\() -> abovePrecedence5)) "|="
    , infixLeft 9 (ParserFast.lazy (\() -> abovePrecedence9)) "<<"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "/="
    , infixLeft 7 (ParserFast.lazy (\() -> abovePrecedence7)) "//"
    , infixLeftWithException 7 (ParserFast.lazy (\() -> abovePrecedence7)) "/" "="
    , infixRight 7 (ParserFast.lazy (\() -> abovePrecedence6)) "</>"
    , infixRight 2 (ParserFast.lazy (\() -> abovePrecedence1)) "||"
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) "<="
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) ">="
    , infixNonAssociative 4 (ParserFast.lazy (\() -> abovePrecedence4)) ">"
    , infixLeft 8 (ParserFast.lazy (\() -> abovePrecedence8)) "<?>"
    , infixNonAssociativeWithException 4 (ParserFast.lazy (\() -> abovePrecedence4)) "<" "|"
    , infixRight 8 (ParserFast.lazy (\() -> abovePrecedence7)) "^"
    ]


expression : Parser (WithComments (Node Expression))
expression =
    extendedSubExpressionMap Basics.identity abovePrecedence0


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (Node Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = Rope.empty
                , syntax =
                    Node
                        -- TODO for v8: don't include extra end width (from bug in elm/parser) in range
                        { start = { row = range.start.row, column = range.start.column - 6 }
                        , end = { row = range.end.row, column = range.end.column + 2 }
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
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> Rope.prependTo elements.comments
                , syntax =
                    Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
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


recordExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
recordExpressionFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2
            (\leftestResult recordAccesses ->
                case recordAccesses of
                    [] ->
                        leftestResult

                    _ :: _ ->
                        { comments = leftestResult.comments
                        , syntax =
                            recordAccesses
                                |> List.foldl
                                    (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                        Node { start = leftRange.start, end = fieldRange.end }
                                            (Expression.RecordAccess leftNode fieldNode)
                                    )
                                    leftestResult.syntax
                        }
            )
            (ParserFast.map2WithRange
                (\range commentsBefore afterCurly ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo afterCurly.comments
                    , syntax = Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                    }
                )
                Layout.maybeLayout
                recordContentsCurlyEnd
            )
            multiRecordAccess
        )


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
            (Layout.maybeLayout |> ParserFast.followedBySymbol "}")
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
    ParserFast.map5WithRange
        (\range name commentsAfterFunctionName commentsAfterEquals expressionResult commentsAfterExpression ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
                    |> Rope.prependTo commentsAfterExpression
            , syntax = Node range ( name, expressionResult.syntax )
            }
        )
        Tokens.functionNameNode
        (Layout.maybeLayout |> ParserFast.followedBySymbol "=")
        Layout.maybeLayout
        expression
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove
        Layout.maybeLayout


literalExpression : Parser (WithComments (Node Expression))
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Node range (Literal string)
            }
        )


charLiteralExpression : Parser (WithComments (Node Expression))
charLiteralExpression =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty
            , syntax = Node range (CharLiteral char)
            }
        )



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    ParserFast.symbolFollowedBy "\\"
        (ParserFast.map5WithStartLocation
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
                    Node
                        { start = { row = start.row, column = start.column - 1 }
                        , end = expressionRange.end
                        }
                        (LambdaExpression
                            { args = firstArg.syntax :: secondUpArgs.syntax
                            , expression = expressionResult.syntax
                            }
                        )
                }
            )
            Layout.maybeLayout
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
        )



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    ParserFast.keywordFollowedBy "case"
        (ParserFast.map5WithStartLocation
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
                        { start = { row = start.row, column = start.column - 4 }
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
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "of" Layout.maybeLayout)
            (ParserFast.withIndentSetToColumn caseStatements)
        )


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
        (Layout.maybeLayout |> ParserFast.followedBySymbol "->")
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
            (Layout.maybeLayout |> ParserFast.followedBySymbol "->")
            Layout.maybeLayout
            expression
        )



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    ParserFast.keywordFollowedBy "let"
        (ParserFast.map3WithStartLocation
            (\start declarations commentsAfterIn expressionResult ->
                let
                    (Node expressionRange _) =
                        expressionResult.syntax
                in
                { comments =
                    declarations.comments
                        |> Rope.prependTo commentsAfterIn
                        |> Rope.prependTo expressionResult.comments
                , syntax =
                    Node
                        { start = { row = start.row, column = start.column - 3 }
                        , end = expressionRange.end
                        }
                        (LetExpression
                            { declarations = declarations.declarations
                            , expression = expressionResult.syntax
                            }
                        )
                }
            )
            (ParserFast.withIndentSetToColumnMinus 3
                (ParserFast.map2
                    (\commentsAfterLet declarations ->
                        { comments =
                            commentsAfterLet
                                |> Rope.prependTo declarations.comments
                        , declarations = declarations.syntax
                        }
                    )
                    Layout.maybeLayout
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
            -- checks that the `in` token used as the end parser in letDeclarationsIn is indented correctly
            (Layout.positivelyIndentedPlusFollowedBy 2
                Layout.maybeLayout
            )
            expression
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
        (Layout.maybeLayout |> ParserFast.followedBySymbol "=")
        Layout.maybeLayout
        expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
            let
                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
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
    ParserFast.floatOrIntOrHexMapWithRange
        (\n range ->
            { comments = Rope.empty
            , syntax = Node range (Floatable n)
            }
        )
        (\n range ->
            { comments = Rope.empty
            , syntax = Node range (Integer n)
            }
        )
        (\n range ->
            { comments = Rope.empty
            , syntax = Node range (Hex n)
            }
        )


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    ParserFast.keywordFollowedBy "if"
        (ParserFast.map8WithStartLocation
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
                    Node
                        { start = { row = start.row, column = start.column - 2 }
                        , end = ifFalseRange.end
                        }
                        (IfBlock
                            condition.syntax
                            ifTrue.syntax
                            ifFalse.syntax
                        )
                }
            )
            Layout.maybeLayout
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "then" Layout.maybeLayout)
            expression
            Layout.maybeLayout
            (ParserFast.keywordFollowedBy "else" Layout.maybeLayout)
            expression
        )


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- TODO only for tests
                    "" ->
                        negationAfterMinus

                    _ ->
                        negationWhitespaceProblem
            )
        )


negationWhitespaceProblem : Parser a
negationWhitespaceProblem =
    ParserFast.problem "if a negation sign is not preceded by whitespace, it's considered subtraction"


negationAfterMinus : Parser (WithComments (Node Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            let
                (Node subExpressionRange _) =
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                Node
                    { start =
                        { row = subExpressionRange.start.row
                        , column = subExpressionRange.start.column - 1
                        }
                    , end = subExpressionRange.end
                    }
                    (Negation subExpressionResult.syntax)
            }
        )
        (ParserFast.lazy (\() -> subExpression))


qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
qualifiedOrVariantOrRecordConstructorReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
            , syntax =
                case after of
                    Nothing ->
                        Node range (FunctionOrValue [] firstName)

                    Just ( qualificationAfter, unqualified, recordAccesses ) ->
                        case recordAccesses of
                            [] ->
                                Node range (FunctionOrValue (firstName :: qualificationAfter) unqualified)

                            (Node firstRecordAccessRange _) :: _ ->
                                let
                                    referenceNode : Node Expression
                                    referenceNode =
                                        Node
                                            { start = range.start
                                            , end =
                                                { row = firstRecordAccessRange.start.row
                                                , column = firstRecordAccessRange.start.column - 1
                                                }
                                            }
                                            (FunctionOrValue (firstName :: qualificationAfter) unqualified)
                                in
                                recordAccesses
                                    |> List.foldl
                                        (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                            Node { start = leftRange.start, end = fieldRange.end }
                                                (Expression.RecordAccess leftNode fieldNode)
                                        )
                                        referenceNode
            }
        )
        Tokens.typeName
        maybeDotReferenceExpressionTuple


maybeDotReferenceExpressionTuple : ParserFast.Parser (Maybe ( List String, String, List (Node String) ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName, [] )

                            Just ( qualificationAfter, unqualified, recordAccess ) ->
                                ( firstName :: qualificationAfter, unqualified, recordAccess )
                    )
                    Tokens.typeName
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                Basics.identity
                (ParserFast.map2
                    (\name recordAccesses ->
                        Just ( [], name, recordAccesses )
                    )
                    Tokens.functionName
                    multiRecordAccess
                )
            )
        )
        Nothing


unqualifiedFunctionReferenceExpressionFollowedByRecordAccess : Parser (WithComments (Node Expression))
unqualifiedFunctionReferenceExpressionFollowedByRecordAccess =
    ParserFast.map2
        (\leftestResult recordAccesses ->
            case recordAccesses of
                [] ->
                    leftestResult

                _ :: _ ->
                    { comments = leftestResult.comments
                    , syntax =
                        recordAccesses
                            |> List.foldl
                                (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                    Node { start = leftRange.start, end = fieldRange.end }
                                        (Expression.RecordAccess leftNode fieldNode)
                                )
                                leftestResult.syntax
                    }
        )
        (Tokens.functionNameMapWithRange
            (\range unqualified ->
                { comments = Rope.empty
                , syntax =
                    Node range (FunctionOrValue [] unqualified)
                }
            )
        )
        multiRecordAccess


recordAccessFunctionExpression : Parser (WithComments (Node Expression))
recordAccessFunctionExpression =
    ParserFast.symbolFollowedBy "."
        (Tokens.functionNameMapWithRange
            (\range field ->
                { comments = Rope.empty
                , syntax =
                    Node (range |> rangeMoveStartLeftByOneColumn)
                        (RecordAccessFunction ("." ++ field))
                }
            )
        )


rangeMoveStartLeftByOneColumn : Range -> Range
rangeMoveStartLeftByOneColumn range =
    { start = { row = range.start.row, column = range.start.column - 1 }
    , end = range.end
    }


tupledExpressionIfNecessaryFollowedByRecordAccess : Parser (WithComments (Node Expression))
tupledExpressionIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf4
            (ParserFast.symbolWithEndLocation ")"
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
            (ParserFast.symbolWithEndLocation "-)"
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
                ParserFast.symbolWithEndLocation
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
    ParserFast.map4WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> Rope.prependTo firstPart.comments
                    |> Rope.prependTo commentsAfterFirstPart
                    |> Rope.prependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesizedFollowedByRecordAccesses recordAccesses ->
                        case recordAccesses of
                            [] ->
                                Node
                                    { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                                    , end = rangeAfterOpeningParens.end
                                    }
                                    (ParenthesizedExpression firstPart.syntax)

                            (Node firstRecordAccessRange _) :: _ ->
                                let
                                    range : Range
                                    range =
                                        { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                                        , end =
                                            { row = firstRecordAccessRange.start.row
                                            , column = firstRecordAccessRange.start.column - 1
                                            }
                                        }

                                    parenthesizedNode : Node Expression
                                    parenthesizedNode =
                                        Node range (ParenthesizedExpression firstPart.syntax)
                                in
                                recordAccesses
                                    |> List.foldl
                                        (\((Node fieldRange _) as fieldNode) ((Node leftRange _) as leftNode) ->
                                            Node { start = leftRange.start, end = fieldRange.end }
                                                (Expression.RecordAccess leftNode fieldNode)
                                        )
                                        parenthesizedNode

                    TupledTwoOrThree ( secondPart, maybeThirdPart ) ->
                        Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case maybeThirdPart of
                                Nothing ->
                                    TupledExpression [ firstPart.syntax, secondPart ]

                                Just thirdPart ->
                                    TupledExpression [ firstPart.syntax, secondPart, thirdPart ]
                            )
            }
        )
        Layout.maybeLayout
        expression
        Layout.maybeLayout
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy ")"
                (multiRecordAccessMap
                    (\recordAccesses -> { comments = Rope.empty, syntax = TupledParenthesizedFollowedByRecordAccesses recordAccesses })
                )
            )
            (ParserFast.map4
                (\commentsBefore partResult commentsAfter maybeThirdPart ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo partResult.comments
                            |> Rope.prependTo commentsAfter
                            |> Rope.prependTo maybeThirdPart.comments
                    , syntax = TupledTwoOrThree ( partResult.syntax, maybeThirdPart.syntax )
                    }
                )
                (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                expression
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                    (ParserFast.map3
                        (\commentsBefore partResult commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo partResult.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax = Just partResult.syntax
                            }
                        )
                        (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                        expression
                        Layout.maybeLayout
                        |> ParserFast.followedBySymbol ")"
                    )
                )
            )
        )


type Tupled
    = TupledParenthesizedFollowedByRecordAccesses (List (Node String))
    | TupledTwoOrThree ( Node Expression, Maybe (Node Expression) )



---


extendedSubExpressionMap :
    (Node Expression -> res)
    -> Parser (WithComments ExtensionRight)
    -> Parser (WithComments res)
extendedSubExpressionMap expressionNodeoRes aboveCurrentPrecedenceLayout =
    ParserFast.map5
        (\commentsBefore leftExpressionResult commentsBeforeExtension maybeArgsReverse extensionsRight ->
            let
                leftMaybeApplied : Node Expression
                leftMaybeApplied =
                    case maybeArgsReverse.syntax of
                        [] ->
                            leftExpressionResult.syntax

                        ((Node lastArgRange _) :: _) as argsReverse ->
                            let
                                ((Node leftRange _) as leftNode) =
                                    leftExpressionResult.syntax
                            in
                            Node { start = leftRange.start, end = lastArgRange.end }
                                (Expression.Application
                                    (leftNode :: List.reverse argsReverse)
                                )
            in
            { comments =
                commentsBefore
                    |> Rope.prependTo leftExpressionResult.comments
                    |> Rope.prependTo commentsBeforeExtension
                    |> Rope.prependTo maybeArgsReverse.comments
                    |> Rope.prependTo extensionsRight.comments
            , syntax =
                List.foldr applyExtensionRight
                    leftMaybeApplied
                    extensionsRight.syntax
                    |> expressionNodeoRes
            }
        )
        Layout.maybeLayout
        (ParserFast.lazy (\() -> subExpression))
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.map2
                (\arg commentsAfter ->
                    { comments = arg.comments |> Rope.prependTo commentsAfter
                    , syntax = arg.syntax
                    }
                )
                (Layout.positivelyIndentedFollowedBy
                    (ParserFast.lazy (\() -> subExpression))
                )
                Layout.optimisticLayout
            )
        )
        (ParserWithComments.manyWithoutReverse
            aboveCurrentPrecedenceLayout
        )


applyExtensionRight : ExtensionRight -> Node Expression -> Node Expression
applyExtensionRight (ExtendRightByOperation extendRightOperation) ((Node { start } _) as leftNode) =
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


infixLeftWithException : Int -> Parser (WithComments ExtensionRight) -> String -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixLeftWithException precedence possibilitiesForPrecedence symbol exceptionSymbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (ParserFast.symbolWithExceptionFollowedBy symbol exceptionSymbol)
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


infixNonAssociativeWithException : Int -> Parser (WithComments ExtensionRight) -> String -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociativeWithException precedence possibilitiesForPrecedence symbol exceptionSymbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (ParserFast.symbolWithExceptionFollowedBy symbol exceptionSymbol)
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


infixHelp :
    Int
    -> Parser (WithComments ExtensionRight)
    -> (Parser (WithComments ExtensionRight) -> Parser (WithComments ExtensionRight))
    -> (Node Expression -> ExtensionRight)
    -> ( Int, Parser (WithComments ExtensionRight) )
infixHelp leftPrecedence rightPrecedence operatorFollowedBy apply =
    ( leftPrecedence
    , operatorFollowedBy
        (extendedSubExpressionMap apply
            rightPrecedence
        )
    )


type ExtensionRight
    = ExtendRightByOperation { symbol : String, direction : Infix.InfixDirection, expression : Node Expression }
