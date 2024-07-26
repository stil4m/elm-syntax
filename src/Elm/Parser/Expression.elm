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
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|.), (|=), Nestable(..), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


subExpressions : Parser (WithComments (Node Expression))
subExpressions =
    Core.lazy (\() -> subExpressionsOneOf)


subExpressionsOneOf : Parser (WithComments (Node Expression))
subExpressionsOneOf =
    Core.oneOf
        [ Core.oneOf
            [ referenceExpression
            , literalExpression
            , numberExpression
            , tupledExpression
            , glslExpression
            , listExpression
            , recordExpression
            , recordAccessFunctionExpression
            , charLiteralExpression
            ]
            |> Node.parser

        -- some possibilities like negate, if (and case?) have weird sub-expression end locations.
        -- Therefore, any expressions with an inner expression at the end likely can't reuse the same outer node range
        -- (note by author of this block: I don't understand where the extra column comes from in negation for example, help appreciated)
        , caseExpression
        , lambdaExpression
        , letExpression
        , ifBlockExpression
        , negationOperation
        ]


andThenOneOf : List ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
andThenOneOf =
    -- TODO Add tests for all operators
    -- TODO Report a syntax error when encountering multiple of the comparison operators
    -- `a < b < c` is not valid Elm syntax
    [ recordAccess
    , infixLeft 1 "|>"
    , infixRight 5 "++"
    , infixRight 1 "<|"
    , infixRight 9 ">>"
    , infixNonAssociative 4 "=="
    , infixLeft 7 "*"
    , infixRight 5 "::"
    , infixLeft 6 "+"
    , infixLeftSubtraction 6
    , infixLeft 6 "|."
    , infixRight 3 "&&"
    , infixLeft 5 "|="
    , infixLeft 9 "<<"
    , infixNonAssociative 4 "/="
    , infixLeft 7 "//"
    , infixLeft 7 "/"
    , infixRight 7 "</>"
    , infixRight 2 "||"
    , infixNonAssociative 4 "<="
    , infixNonAssociative 4 ">="
    , infixNonAssociative 4 ">"
    , infixLeft 8 "<?>"
    , infixNonAssociative 4 "<"
    , infixRight 8 "^"

    -- function application must be last
    -- TODO validate function application arguments (issue #209)
    , functionCall
    ]


expression : Parser (WithComments (Node Expression))
expression =
    subExpression 0


recordAccess : ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
recordAccess =
    postfix 100
        recordAccessParser
        (\((Node { start } _) as left) ((Node { end } _) as field) ->
            Node
                { start = start, end = end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Parser (WithComments (Node String))
recordAccessParser =
    lookBehindOneCharacter
        |> Core.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    problemRecordAccessStartingWithSpace

                else
                    dotField
            )
        |> ParserWithComments.fromCore


problemRecordAccessStartingWithSpace : Core.Parser a
problemRecordAccessStartingWithSpace =
    Core.problem "Record access can't start with a space"


dotField : Core.Parser (Node String)
dotField =
    Tokens.dot
        |> Parser.Extra.continueWith (Node.parserCore Tokens.functionName)


functionCall : ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
functionCall =
    infixLeftWithState 90
        (Layout.positivelyIndented ())
        (\((Node { start } leftValue) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (case leftValue of
                    Expression.Application args ->
                        Expression.Application (args ++ [ right ])

                    _ ->
                        Expression.Application [ left, right ]
                )
        )


glslStart : String
glslStart =
    "[glsl|"


glslStartLength : Int
glslStartLength =
    String.length glslStart


glslEnd : String
glslEnd =
    "|]"


glslExpression : Parser (WithComments Expression)
glslExpression =
    Core.mapChompedString
        (\s () -> s |> String.dropLeft glslStartLength |> GLSLExpression)
        (Core.multiComment glslStart glslEnd NotNestable)
        |. Core.symbol glslEnd
        |> ParserWithComments.fromCore


listExpression : Parser (WithComments Expression)
listExpression =
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBefore ->
                    \elements ->
                        { comments = Rope.flatFromList [ commentsBefore, elements.comments ]
                        , syntax = ListExpr elements.syntax
                        }
                )
                Layout.maybeLayout
            )
    )
        |= ParserWithComments.sepBy "," expression
        |. Tokens.squareEnd



-- recordExpression


recordExpression : Parser (WithComments Expression)
recordExpression =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBefore ->
                    \afterCurly ->
                        { comments = Rope.flatFromList [ commentsBefore, afterCurly.comments ]
                        , syntax = afterCurly.syntax
                        }
                )
                Layout.maybeLayout
            )
    )
        |= ParserWithComments.maybeMap identity
            (RecordExpr [])
            recordContents
        |. Tokens.curlyEnd


recordContents : Parser (WithComments Expression)
recordContents =
    Node.parserCoreMap
        (\nameNode ->
            \commentsAfterFunctionName ->
                \afterNameBeforeFields ->
                    \tailFields ->
                        \commentsAfterEverything ->
                            { comments =
                                Rope.flatFromList
                                    [ commentsAfterFunctionName
                                    , afterNameBeforeFields.comments
                                    , tailFields.comments
                                    , commentsAfterEverything
                                    ]
                            , syntax =
                                case afterNameBeforeFields.syntax of
                                    RecordUpdateFirstSetter firstField ->
                                        RecordUpdateExpression nameNode (firstField :: tailFields.syntax)

                                    FieldsFirstValue firstFieldValue ->
                                        RecordExpr (Node.combine Tuple.pair nameNode firstFieldValue :: tailFields.syntax)
                            }
        )
        Tokens.functionName
        |= Layout.maybeLayout
        |= Core.oneOf
            [ (Tokens.pipe
                |> Parser.Extra.continueWith
                    (Core.map
                        (\commentsBefore ->
                            \setterResult ->
                                { comments = Rope.flatFromList [ commentsBefore, setterResult.comments ]
                                , syntax = RecordUpdateFirstSetter setterResult.syntax
                                }
                        )
                        Layout.maybeLayout
                    )
              )
                |= recordSetterNodeWithLayout
            , (Tokens.equal
                |> Parser.Extra.continueWith
                    (Core.map
                        (\commentsBefore ->
                            \expressionResult ->
                                { comments = Rope.flatFromList [ commentsBefore, expressionResult.comments ]
                                , syntax = FieldsFirstValue expressionResult.syntax
                                }
                        )
                        Layout.maybeLayout
                    )
              )
                |= expression
            ]
        |= recordFields
        |= Layout.maybeLayout


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        ((Tokens.comma
            |> Parser.Extra.continueWith
                (Core.map
                    (\commentsBefore ->
                        \setterResult ->
                            { comments = Rope.flatFromList [ commentsBefore, setterResult.comments ]
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
    Node.parserCoreMap
        (\((Node fnNameRange _) as fnName) ->
            \commentsAfterFunctionName ->
                \commentsAfterEquals ->
                    \expressionResult ->
                        \commentsAfterExpression ->
                            \( endRow, endColumn ) ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterFunctionName
                                        , commentsAfterEquals
                                        , expressionResult.comments
                                        , commentsAfterExpression
                                        ]
                                , syntax =
                                    Node { start = fnNameRange.start, end = { row = endRow, column = endColumn } }
                                        ( fnName, expressionResult.syntax )
                                }
        )
        Tokens.functionName
        |= Layout.maybeLayout
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression
        |= Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        |= Core.getPosition


literalExpression : Parser (WithComments Expression)
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteral
        |> ParserWithComments.fromCoreMap Literal


charLiteralExpression : Parser (WithComments Expression)
charLiteralExpression =
    Tokens.characterLiteral
        |> ParserWithComments.fromCoreMap CharLiteral



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    Core.map
        (\( startRow, startColumn ) ->
            \commentsAfterBackslash ->
                \argsResult ->
                    \commentsBeforeArrowRight ->
                        \expressionResult ->
                            let
                                (Node { end } _) =
                                    expressionResult.syntax
                            in
                            { comments =
                                Rope.flatFromList
                                    [ commentsAfterBackslash
                                    , argsResult.comments
                                    , commentsBeforeArrowRight
                                    , expressionResult.comments
                                    ]
                            , syntax =
                                { args = argsResult.syntax, expression = expressionResult.syntax }
                                    |> LambdaExpression
                                    |> Node { start = { row = startRow, column = startColumn }, end = end }
                            }
        )
        Core.getPosition
        |. Tokens.backSlash
        |= Layout.maybeLayout
        |= ParserWithComments.sepBy1WithState Layout.maybeLayout Patterns.pattern
        |= Layout.maybeLayout
        |. Tokens.arrowRight
        |= expression



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    (Core.map
        (\( startRow, startColumn ) ->
            \caseBlock_ ->
                \( ( _, Node firstCaseExpressionRange _ ) as firstCase, lastToSecondCase ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end =
                            case lastToSecondCase of
                                [] ->
                                    firstCaseExpressionRange.end

                                ( _, Node lastCaseExpressionRange _ ) :: _ ->
                                    lastCaseExpressionRange.end
                        }
                        (CaseExpression
                            { expression = caseBlock_
                            , cases = firstCase :: List.reverse lastToSecondCase
                            }
                        )
        )
        Core.getPosition
        |. Tokens.caseToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.keep expression
    )
        |. Layout.positivelyIndentedCore
        |. Tokens.ofToken
        |> ParserWithComments.ignore Layout.layout
        |> ParserWithComments.keep (Parser.Extra.withIndent caseStatements)


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    ParserWithComments.map
        (\firstCase ->
            \lastToSecondCase ->
                ( firstCase
                , lastToSecondCase
                )
        )
        caseStatement
        |> ParserWithComments.keep
            (ParserWithComments.manyWithoutReverse caseStatement)


caseStatement : Parser (WithComments Case)
caseStatement =
    Layout.onTopIndentation
        (\pattern ->
            \commentsBeforeArrowRight ->
                \commentsAfterArrowRight ->
                    \expr ->
                        { comments =
                            Rope.flatFromList
                                [ pattern.comments
                                , commentsBeforeArrowRight
                                , commentsAfterArrowRight
                                , expr.comments
                                ]
                        , syntax = ( pattern.syntax, expr.syntax )
                        }
        )
        |= Patterns.pattern
        |= Layout.maybeLayout
        |. Tokens.arrowRight
        |= Layout.maybeLayout
        |= expression



-- Let Expression


letExpression : Parser (WithComments (Node Expression))
letExpression =
    Parser.Extra.withIndent
        ((Core.map
            (\( startRow, startColumn ) ->
                \declarations ->
                    \((Node { end } _) as expr) ->
                        Node { start = { row = startRow, column = startColumn }, end = end }
                            (LetExpression { declarations = declarations, expression = expr })
            )
            Core.getPosition
            |. Tokens.letToken
            |> ParserWithComments.fromCoreIgnore Layout.layout
            |> ParserWithComments.keep (Parser.Extra.withIndent letDeclarations)
            |> ParserWithComments.ignore Layout.optimisticLayout
         )
            |. Tokens.inToken
        )
        |> ParserWithComments.keep expression


letDeclarations : Parser (WithComments (List (Node LetDeclaration)))
letDeclarations =
    ParserWithComments.map (\head -> \tail -> head :: tail) blockElement
        |> ParserWithComments.keep (ParserWithComments.many blockElement)


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentation ()
        |> Parser.Extra.continueWith
            (Core.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    ParserWithComments.map
        (\((Node { start } _) as pattern) ->
            \((Node { end } _) as expr) ->
                Node { start = start, end = end } (LetDestructuring pattern expr)
        )
        Patterns.pattern
        |. Tokens.equal
        |> ParserWithComments.keep expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
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
                                    (LetFunction
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
                                            (LetFunction
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
        |> ParserWithComments.keep (ParserWithComments.many (Patterns.pattern |> ParserWithComments.ignore Layout.maybeLayout))
    )
        |. Tokens.equal
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep expression
        |> ParserWithComments.flattenFromCore


numberExpression : Parser (WithComments Expression)
numberExpression =
    Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex
        |> ParserWithComments.fromCore


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    Core.map
        (\( startRow, startColumn ) ->
            \condition ->
                \ifTrue ->
                    \commentsAfterElse ->
                        \ifFalse ->
                            let
                                (Node { end } _) =
                                    ifFalse.syntax
                            in
                            { comments =
                                Rope.flatFromList
                                    [ condition.comments
                                    , ifTrue.comments
                                    , commentsAfterElse
                                    , ifFalse.comments
                                    ]
                            , syntax =
                                Node
                                    { start = { row = startRow, column = startColumn }, end = end }
                                    (IfBlock condition.syntax ifTrue.syntax ifFalse.syntax)
                            }
        )
        Core.getPosition
        |. Tokens.ifToken
        |= expression
        |. Tokens.thenToken
        |= expression
        |. Tokens.elseToken
        |= Layout.layout
        |= expression


negationOperation : Parser (WithComments (Node Expression))
negationOperation =
    minusNotFollowedBySpace
        |> Parser.Extra.continueWith
            (ParserWithComments.map
                (\((Node { start, end } _) as subExpr) ->
                    Node
                        { start = { row = start.row, column = start.column - 1 }, end = end }
                        (Negation subExpr)
                )
                (subExpression 95)
            )


minusNotFollowedBySpace : Core.Parser ()
minusNotFollowedBySpace =
    Core.backtrackable Tokens.minus
        |> Parser.Extra.continueWith
            (Core.oneOf
                [ Core.chompIf (\next -> next == '\u{000D}' || next == '\n' || next == ' ')
                    |> Core.backtrackable
                    |> Core.map (\() -> problemNegationThenSpace)
                , Core.succeed (Core.commit ())
                ]
            )
        |> Core.andThen identity


problemNegationThenSpace : Core.Parser a
problemNegationThenSpace =
    Core.problem "negation sign cannot be followed by a space"


referenceExpression : Parser (WithComments Expression)
referenceExpression =
    ParserWithComments.fromCoreMap
        (\( qualification, unqualified ) ->
            FunctionOrValue qualification unqualified
        )
        referenceExpressionTuple


referenceExpressionTuple : Core.Parser ( List String, String )
referenceExpressionTuple =
    Core.oneOf
        [ Core.map
            (\firstName ->
                \after ->
                    case after of
                        Nothing ->
                            ( [], firstName )

                        Just ( qualificationAfter, unqualified ) ->
                            ( firstName :: qualificationAfter, unqualified )
            )
            Tokens.typeName
            |= Core.oneOf
                [ Core.map (\() -> Just) Tokens.dot
                    |= Core.lazy (\() -> referenceExpressionTuple)
                , Core.succeed Nothing
                ]
        , Core.map (\unqualified -> ( [], unqualified )) Tokens.functionName
        ]


recordAccessFunctionExpression : Parser (WithComments Expression)
recordAccessFunctionExpression =
    Tokens.dot
        |> Parser.Extra.continueWith Tokens.functionName
        |> ParserWithComments.fromCoreMap (\field -> RecordAccessFunction ("." ++ field))


tupledExpression : Parser (WithComments Expression)
tupledExpression =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Core.oneOf
                ((Tokens.parensEnd |> ParserWithComments.fromCoreMap (\() -> UnitExpr))
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (Core.symbol "-)" |> ParserWithComments.fromCoreMap (\() -> PrefixOperator "-"))
                    :: (tupledExpressionInnerNested |. Tokens.parensEnd)
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser (WithComments Expression))
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                Core.symbol (allowedOperatorToken ++ ")")
                    |> ParserWithComments.fromCoreMap (\() -> PrefixOperator allowedOperatorToken)
            )


tupledExpressionInnerNested : Parser (WithComments Expression)
tupledExpressionInnerNested =
    ParserWithComments.map
        (\firstPart ->
            \tailPartsReverse ->
                case tailPartsReverse of
                    [] ->
                        ParenthesizedExpression firstPart

                    _ ->
                        TupledExpression (firstPart :: List.reverse tailPartsReverse)
        )
        expression
        |> ParserWithComments.keep
            (ParserWithComments.manyWithoutReverse
                (Tokens.comma
                    |> Parser.Extra.continueWith expression
                )
            )



---


subExpression : Int -> Parser (WithComments (Node Expression))
subExpression currentPrecedence =
    let
        parser : Node Expression -> Parser (WithComments (Core.Step (Node Expression) (Node Expression)))
        parser =
            expressionHelp currentPrecedence
    in
    optimisticLayoutSubExpressions
        |> ParserWithComments.andThen
            (\leftExpression -> ParserWithComments.loop leftExpression parser)


optimisticLayoutSubExpressions : Parser (WithComments (Node Expression))
optimisticLayoutSubExpressions =
    Core.map
        (\commentsBefore ->
            \subExpressionResult ->
                { comments = Rope.flatFromList [ commentsBefore, subExpressionResult.comments ]
                , syntax = subExpressionResult.syntax
                }
        )
        Layout.optimisticLayout
        |= subExpressions


expressionHelp : Int -> Node Expression -> Parser (WithComments (Core.Step (Node Expression) (Node Expression)))
expressionHelp currentPrecedence leftExpression =
    case getAndThenOneOfAbovePrecedence currentPrecedence of
        Just parser ->
            Core.map
                (\commentsBefore ->
                    \combineExpressionResult ->
                        { comments = Rope.flatFromList [ commentsBefore, combineExpressionResult.comments ]
                        , syntax = combineExpressionResult.syntax
                        }
                )
                Layout.optimisticLayout
                |= ParserWithComments.maybeMap Core.Loop
                    (Core.Done leftExpression)
                    (combineOneOfApply parser leftExpression)

        Nothing ->
            Core.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


combineOneOfApply :
    List (arg -> Parser (WithComments arg))
    -> arg
    -> Parser (WithComments arg)
combineOneOfApply possibilitiesForCurrentPrecedence leftExpression =
    Core.oneOf
        (List.map
            (\parser -> parser leftExpression)
            possibilitiesForCurrentPrecedence
        )


getAndThenOneOfAbovePrecedence : Int -> Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
getAndThenOneOfAbovePrecedence precedence =
    case precedence of
        0 ->
            Just andThenOneOfAbovePrecedence0

        1 ->
            Just andThenOneOfAbovePrecedence1

        2 ->
            Just andThenOneOfAbovePrecedence2

        3 ->
            Just andThenOneOfAbovePrecedence3

        4 ->
            Just andThenOneOfAbovePrecedence4

        5 ->
            Just andThenOneOfAbovePrecedence5

        6 ->
            Just andThenOneOfAbovePrecedence6

        7 ->
            Just andThenOneOfAbovePrecedence7

        8 ->
            Just andThenOneOfAbovePrecedence8

        9 ->
            Just andThenOneOfAbovePrecedence9

        90 ->
            Just andThenOneOfAbovePrecedence90

        95 ->
            Just andThenOneOfAbovePrecedence95

        100 ->
            Just andThenOneOfAbovePrecedence100

        _ ->
            Nothing


andThenOneOfAbovePrecedence0 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence0 =
    computeAndThenOneOfAbovePrecedence 0


andThenOneOfAbovePrecedence1 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence1 =
    computeAndThenOneOfAbovePrecedence 1


andThenOneOfAbovePrecedence2 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence2 =
    computeAndThenOneOfAbovePrecedence 2


andThenOneOfAbovePrecedence3 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence3 =
    computeAndThenOneOfAbovePrecedence 3


andThenOneOfAbovePrecedence4 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence4 =
    computeAndThenOneOfAbovePrecedence 4


andThenOneOfAbovePrecedence5 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence5 =
    computeAndThenOneOfAbovePrecedence 5


andThenOneOfAbovePrecedence6 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence6 =
    computeAndThenOneOfAbovePrecedence 6


andThenOneOfAbovePrecedence7 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence7 =
    computeAndThenOneOfAbovePrecedence 7


andThenOneOfAbovePrecedence8 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence8 =
    computeAndThenOneOfAbovePrecedence 8


andThenOneOfAbovePrecedence9 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence9 =
    computeAndThenOneOfAbovePrecedence 9


andThenOneOfAbovePrecedence90 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence90 =
    computeAndThenOneOfAbovePrecedence 90


andThenOneOfAbovePrecedence95 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence95 =
    computeAndThenOneOfAbovePrecedence 95


andThenOneOfAbovePrecedence100 : List (Node Expression -> Parser (WithComments (Node Expression)))
andThenOneOfAbovePrecedence100 =
    computeAndThenOneOfAbovePrecedence 100


computeAndThenOneOfAbovePrecedence : Int -> List (Node Expression -> Parser (WithComments (Node Expression)))
computeAndThenOneOfAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )


infixLeft : Int -> String -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixLeft precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixRight precedence symbol =
    infixRightHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Right left right)
        )


lookBehindOneCharacter : Core.Parser String
lookBehindOneCharacter =
    Core.map (\offset -> \source -> String.slice (offset - 1) offset source)
        Core.getOffset
        |= Core.getSource


infixLeftSubtraction : Int -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixLeftSubtraction precedence =
    infixLeftHelp precedence
        (lookBehindOneCharacter
            |> Core.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        Tokens.minusSymbols

                    else
                        Tokens.minus
                )
        )
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication "-" Infix.Left left right)
        )


infixLeftHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixLeftHelp precedence p apply =
    infixHelp precedence precedence p apply


infixLeftWithState : Int -> Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixLeftWithState precedence operator apply =
    let
        parser : Parser (WithComments (Node Expression))
        parser =
            subExpression precedence
    in
    ( precedence
    , \left ->
        operator
            |> Parser.Extra.continueWith
                (ParserWithComments.map (\e -> apply left e) parser)
    )


infixRightHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixHelp leftPrecedence rightPrecedence operator apply =
    let
        parser : Parser (WithComments (Node Expression))
        parser =
            subExpression rightPrecedence
    in
    ( leftPrecedence
    , \left ->
        Core.map (\() -> \e -> apply left e)
            operator
            |> ParserWithComments.fromCoreKeep parser
    )


postfix : Int -> Parser (WithComments a) -> (expr -> a -> expr) -> ( Int, expr -> Parser (WithComments expr) )
postfix precedence operator apply =
    ( precedence
    , \left -> ParserWithComments.map (\right -> apply left right) operator
    )
