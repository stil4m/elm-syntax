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
import Parser exposing ((|.), (|=), Nestable(..), Parser)
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpressions : Parser (WithComments (Node Expression))
subExpressions =
    Parser.lazy (\() -> subExpressionsOneOf)


subExpressionsOneOf : Parser (WithComments (Node Expression))
subExpressionsOneOf =
    Parser.oneOf
        [ Parser.oneOf
            [ referenceExpression
            , literalExpression
            , numberExpression
                |> Parser.map (\n -> { comments = Rope.empty, syntax = n })
            , tupledExpression
            , glslExpression
                |> Parser.map (\glsl -> { comments = Rope.empty, syntax = glsl })
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


glslExpression : Parser Expression
glslExpression =
    Parser.mapChompedString
        (\s () -> s |> String.dropLeft glslStartLength |> GLSLExpression)
        (Parser.multiComment glslStart glslEnd NotNestable)
        |. Parser.symbol glslEnd


listExpression : Parser (WithComments Expression)
listExpression =
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Parser.map
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
            (Parser.map
                (\commentsBefore ->
                    \maybeAfterCurly ->
                        case maybeAfterCurly of
                            Nothing ->
                                { comments = commentsBefore
                                , syntax = RecordExpr []
                                }

                            Just afterCurly ->
                                { comments = Rope.flatFromList [ commentsBefore, afterCurly.comments ]
                                , syntax = afterCurly.syntax
                                }
                )
                Layout.maybeLayout
            )
    )
        |= Parser.oneOf
            [ Parser.map Just recordContents
            , Parser.succeed Nothing
            ]
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
        |= Parser.oneOf
            [ (Tokens.pipe
                |> Parser.Extra.continueWith
                    (Parser.map
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
                    (Parser.map
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
                (Parser.map
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
        |= Parser.getPosition


literalExpression : Parser (WithComments Expression)
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteral
        |> Parser.map (\string -> { comments = Rope.empty, syntax = Literal string })


charLiteralExpression : Parser (WithComments Expression)
charLiteralExpression =
    Tokens.characterLiteral
        |> Parser.map (\char -> { comments = Rope.empty, syntax = CharLiteral char })



-- lambda


lambdaExpression : Parser (WithComments (Node Expression))
lambdaExpression =
    Parser.map
        (\( startRow, startColumn ) ->
            \commentsAfterBackslash ->
                \firstArg ->
                    \secondUpArgs ->
                        \commentsBeforeArrowRight ->
                            \expressionResult ->
                                let
                                    (Node { end } _) =
                                        expressionResult.syntax
                                in
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterBackslash
                                        , firstArg.comments
                                        , secondUpArgs.comments
                                        , commentsBeforeArrowRight
                                        , expressionResult.comments
                                        ]
                                , syntax =
                                    { args = firstArg.syntax :: secondUpArgs.syntax
                                    , expression = expressionResult.syntax
                                    }
                                        |> LambdaExpression
                                        |> Node { start = { row = startRow, column = startColumn }, end = end }
                                }
        )
        Parser.getPosition
        |. Tokens.backSlash
        |= Layout.maybeLayout
        |= Patterns.pattern
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \patternResult ->
                        { comments = Rope.flatFromList [ commentsBefore, patternResult.comments ]
                        , syntax = patternResult.syntax
                        }
                )
                Layout.maybeLayout
                |= Patterns.pattern
            )
        |= Layout.maybeLayout
        |. Tokens.arrowRight
        |= expression



-- Case Expression


caseExpression : Parser (WithComments (Node Expression))
caseExpression =
    (Parser.map
        (\( startRow, startColumn ) ->
            \commentsAfterCase ->
                \casedExpressionResult ->
                    \commentsAfterOf ->
                        \casesResult ->
                            let
                                ( ( _, Node firstCaseExpressionRange _ ) as firstCase, lastToSecondCase ) =
                                    casesResult.syntax
                            in
                            { comments =
                                Rope.flatFromList
                                    [ commentsAfterCase
                                    , casedExpressionResult.comments
                                    , commentsAfterOf
                                    , casesResult.comments
                                    ]
                            , syntax =
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
                                        { expression = casedExpressionResult.syntax
                                        , cases = firstCase :: List.reverse lastToSecondCase
                                        }
                                    )
                            }
        )
        Parser.getPosition
        |. Tokens.caseToken
        |= Layout.layout
        |= expression
    )
        |. Layout.positivelyIndentedCore
        |. Tokens.ofToken
        |= Layout.layout
        |= Parser.Extra.withIndent caseStatements


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    Parser.map
        (\firstCase ->
            \lastToSecondCase ->
                { comments = Rope.flatFromList [ firstCase.comments, lastToSecondCase.comments ]
                , syntax =
                    ( firstCase.syntax
                    , lastToSecondCase.syntax
                    )
                }
        )
        caseStatement
        |= ParserWithComments.manyWithoutReverse caseStatement


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
        (Parser.map
            (\( startRow, startColumn ) ->
                \commentsAfterLet ->
                    \declarations ->
                        \commentsBeforeIn ->
                            \expressionResult ->
                                let
                                    ((Node { end } _) as expr) =
                                        expressionResult.syntax
                                in
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterLet
                                        , declarations.comments
                                        , commentsBeforeIn
                                        , expressionResult.comments
                                        ]
                                , syntax =
                                    Node { start = { row = startRow, column = startColumn }, end = end }
                                        (LetExpression { declarations = declarations.syntax, expression = expr })
                                }
            )
            Parser.getPosition
            |. Tokens.letToken
            |= Layout.layout
            |= Parser.Extra.withIndent letDeclarations
            |= Layout.optimisticLayout
            |. Tokens.inToken
        )
        |= expression


letDeclarations : Parser (WithComments (List (Node LetDeclaration)))
letDeclarations =
    Parser.map
        (\head ->
            \tail ->
                { comments = Rope.flatFromList [ head.comments, tail.comments ]
                , syntax = head.syntax :: tail.syntax
                }
        )
        blockElement
        |= ParserWithComments.many blockElement


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentation ()
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )


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
                { comments = Rope.flatFromList [ pattern.comments, expressionResult.comments ]
                , syntax =
                    Node { start = start, end = end }
                        (LetDestructuring pattern.syntax expressionResult.syntax)
                }
        )
        Patterns.pattern
        |. Tokens.equal
        |= expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    (Node.parserCoreMap
        (\((Node { start } startName) as startNameNode) ->
            \commentsAfterStartName ->
                \maybeSignature ->
                    \arguments ->
                        \commentsAfterEqual ->
                            \expressionResult ->
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
                                            , expressionResult.comments
                                            ]
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
    )
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression
        |> Parser.andThen identity


numberExpression : Parser Expression
numberExpression =
    Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex


ifBlockExpression : Parser (WithComments (Node Expression))
ifBlockExpression =
    Parser.map
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
        Parser.getPosition
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
            (Parser.map
                (\subExpressionResult ->
                    let
                        (Node { start, end } _) =
                            subExpressionResult.syntax
                    in
                    { subExpressionResult
                        | syntax =
                            Node
                                { start = { row = start.row, column = start.column - 1 }, end = end }
                                (Negation subExpressionResult.syntax)
                    }
                )
                (subExpression 95)
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


referenceExpression : Parser (WithComments Expression)
referenceExpression =
    Parser.map
        (\( qualification, unqualified ) ->
            { comments = Rope.empty, syntax = FunctionOrValue qualification unqualified }
        )
        referenceExpressionTuple


referenceExpressionTuple : Parser.Parser ( List String, String )
referenceExpressionTuple =
    Parser.oneOf
        [ Parser.map
            (\firstName ->
                \after ->
                    case after of
                        Nothing ->
                            ( [], firstName )

                        Just ( qualificationAfter, unqualified ) ->
                            ( firstName :: qualificationAfter, unqualified )
            )
            Tokens.typeName
            |= Parser.oneOf
                [ Tokens.dot
                    |> Parser.Extra.continueWith
                        (Parser.map Just
                            (Parser.lazy (\() -> referenceExpressionTuple))
                        )
                , Parser.succeed Nothing
                ]
        , Parser.map (\unqualified -> ( [], unqualified )) Tokens.functionName
        ]


recordAccessFunctionExpression : Parser (WithComments Expression)
recordAccessFunctionExpression =
    Tokens.dot
        |> Parser.Extra.continueWith Tokens.functionName
        |> Parser.map
            (\field ->
                { comments = Rope.empty
                , syntax = RecordAccessFunction ("." ++ field)
                }
            )


tupledExpression : Parser (WithComments Expression)
tupledExpression =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                ((Tokens.parensEnd |> Parser.map (\() -> unitWithComments))
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (Parser.symbol "-)" |> Parser.map (\() -> minusPrefixOperatorWithComments))
                    :: (tupledExpressionInnerNested |. Tokens.parensEnd)
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )


unitWithComments : WithComments Expression
unitWithComments =
    { comments = Rope.empty, syntax = UnitExpr }


minusPrefixOperatorWithComments : WithComments Expression
minusPrefixOperatorWithComments =
    { comments = Rope.empty, syntax = PrefixOperator "-" }


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser (WithComments Expression))
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                Parser.symbol (allowedOperatorToken ++ ")")
                    |> Parser.map (\() -> { comments = Rope.empty, syntax = PrefixOperator allowedOperatorToken })
            )


tupledExpressionInnerNested : Parser (WithComments Expression)
tupledExpressionInnerNested =
    Parser.map
        (\firstPart ->
            \tailPartsReverse ->
                case tailPartsReverse.syntax of
                    [] ->
                        { comments = firstPart.comments
                        , syntax = ParenthesizedExpression firstPart.syntax
                        }

                    _ ->
                        { comments = Rope.flatFromList [ firstPart.comments, tailPartsReverse.comments ]
                        , syntax = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                        }
        )
        expression
        |= ParserWithComments.manyWithoutReverse
            (Tokens.comma
                |> Parser.Extra.continueWith expression
            )



---


subExpression : Int -> Parser (WithComments (Node Expression))
subExpression currentPrecedence =
    let
        parser : WithComments (Node Expression) -> Parser (Parser.Step (WithComments (Node Expression)) (WithComments (Node Expression)))
        parser leftExpression =
            expressionHelp currentPrecedence leftExpression
    in
    optimisticLayoutSubExpressions
        |> Parser.andThen
            (\leftExpression -> Parser.loop leftExpression parser)


optimisticLayoutSubExpressions : Parser (WithComments (Node Expression))
optimisticLayoutSubExpressions =
    Parser.map
        (\commentsBefore ->
            \subExpressionResult ->
                { comments = Rope.flatFromList [ commentsBefore, subExpressionResult.comments ]
                , syntax = subExpressionResult.syntax
                }
        )
        Layout.optimisticLayout
        |= subExpressions


expressionHelp : Int -> WithComments (Node Expression) -> Parser (Parser.Step (WithComments (Node Expression)) (WithComments (Node Expression)))
expressionHelp currentPrecedence leftExpression =
    case getAndThenOneOfAbovePrecedence currentPrecedence of
        Just parser ->
            Parser.map
                (\commentsBefore ->
                    \maybeCombineExpressionResult ->
                        case maybeCombineExpressionResult of
                            Nothing ->
                                Parser.Done
                                    { comments = Rope.flatFromList [ leftExpression.comments, commentsBefore ]
                                    , syntax = leftExpression.syntax
                                    }

                            Just combineExpressionResult ->
                                Parser.Loop
                                    { comments =
                                        Rope.flatFromList
                                            [ leftExpression.comments
                                            , commentsBefore
                                            , combineExpressionResult.comments
                                            ]
                                    , syntax = combineExpressionResult.syntax
                                    }
                )
                Layout.optimisticLayout
                |= Parser.oneOf
                    [ Parser.map Just
                        (combineOneOfApply parser leftExpression.syntax)
                    , Parser.succeed Nothing
                    ]

        Nothing ->
            Parser.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


combineOneOfApply :
    List (arg -> Parser (WithComments arg))
    -> arg
    -> Parser (WithComments arg)
combineOneOfApply possibilitiesForCurrentPrecedence leftExpression =
    Parser.oneOf
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
        (Parser.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Parser.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixRight precedence symbol =
    infixRightHelp precedence
        (Parser.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Right left right)
        )


lookBehindOneCharacter : Parser.Parser String
lookBehindOneCharacter =
    Parser.map (\offset -> \source -> String.slice (offset - 1) offset source)
        Parser.getOffset
        |= Parser.getSource


infixLeftSubtraction : Int -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixLeftSubtraction precedence =
    infixLeftHelp precedence
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
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication "-" Infix.Left left right)
        )


infixLeftHelp : Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
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
                (Parser.map (\e -> { e | syntax = apply left e.syntax })
                    parser
                )
    )


infixRightHelp : Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
infixHelp leftPrecedence rightPrecedence operator apply =
    let
        parser : Parser (WithComments (Node Expression))
        parser =
            subExpression rightPrecedence
    in
    ( leftPrecedence
    , \left ->
        operator
            |> Parser.Extra.continueWith
                (Parser.map (\e -> { e | syntax = apply left e.syntax })
                    parser
                )
    )


postfix : Int -> Parser a -> (expr -> a -> expr) -> ( Int, expr -> Parser (WithComments expr) )
postfix precedence operator apply =
    ( precedence
    , \left ->
        Parser.map
            (\right ->
                { comments = Rope.empty
                , syntax = apply left right
                }
            )
            operator
    )
