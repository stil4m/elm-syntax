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
import Elm.Syntax.Range exposing (Location)
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


functionCall : ( Int, Node Expression -> Parser (WithComments (Node Expression)) )
functionCall =
    infixLeftWithState 90
        Layout.positivelyIndented
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
    Parser.mapChompedString
        (\s () ->
            { comments = Rope.empty
            , syntax =
                s |> String.dropLeft glslStartLength |> GLSLExpression
            }
        )
        (Parser.multiComment glslStart glslEnd NotNestable)
        |. Parser.symbol glslEnd


listExpression : Parser (WithComments Expression)
listExpression =
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBefore ->
                    \maybeElements ->
                        case maybeElements of
                            Nothing ->
                                { comments = commentsBefore, syntax = expressionListEmpty }

                            Just elements ->
                                { comments = commentsBefore |> Rope.prependTo elements.comments
                                , syntax = ListExpr elements.syntax
                                }
                )
                Layout.maybeLayout
            )
    )
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


expressionListEmpty : Expression
expressionListEmpty =
    ListExpr []



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
                                , syntax = expressionRecordEmpty
                                }

                            Just afterCurly ->
                                { comments = commentsBefore |> Rope.prependTo afterCurly.comments
                                , syntax = afterCurly.syntax
                                }
                )
                Layout.maybeLayout
            )
    )
        |= recordContents
        |. Tokens.curlyEnd


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
                                \commentsAfterEverything ->
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
                                                |> Rope.prependTo commentsAfterEverything
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
                                    { comments = commentsBefore |> Rope.prependTo expressionResult.comments
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
                                    commentsAfterBackslash
                                        |> Rope.prependTo firstArg.comments
                                        |> Rope.prependTo secondUpArgs.comments
                                        |> Rope.prependTo commentsBeforeArrowRight
                                        |> Rope.prependTo expressionResult.comments
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
                        { comments = commentsBefore |> Rope.prependTo patternResult.comments
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
    Parser.map
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
                                commentsAfterCase
                                    |> Rope.prependTo casedExpressionResult.comments
                                    |> Rope.prependTo commentsAfterOf
                                    |> Rope.prependTo casesResult.comments
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
        |. Layout.positivelyIndented
        |. Tokens.ofToken
        |= Layout.layout
        |= Parser.Extra.withIndent caseStatements


caseStatements : Parser (WithComments ( Case, List Case ))
caseStatements =
    Parser.map
        (\firstCase ->
            \lastToSecondCase ->
                { comments = firstCase.comments |> Rope.prependTo lastToSecondCase.comments
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
                            pattern.comments
                                |> Rope.prependTo commentsBeforeArrowRight
                                |> Rope.prependTo commentsAfterArrowRight
                                |> Rope.prependTo expr.comments
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
                                    commentsAfterLet
                                        |> Rope.prependTo declarations.comments
                                        |> Rope.prependTo commentsBeforeIn
                                        |> Rope.prependTo expressionResult.comments
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
                { comments = head.comments |> Rope.prependTo tail.comments
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
                { comments = pattern.comments |> Rope.prependTo expressionResult.comments
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
        |= ParserWithComments.many
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
        |. Tokens.equal
        |= Layout.maybeLayout
        |= expression
        |> Parser.andThen identity


numberExpression : Parser (WithComments Expression)
numberExpression =
    Elm.Parser.Numbers.forgivingNumber
        (\n -> { comments = Rope.empty, syntax = Floatable n })
        (\n -> { comments = Rope.empty, syntax = Integer n })
        (\n -> { comments = Rope.empty, syntax = Hex n })


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
                                condition.comments
                                    |> Rope.prependTo ifTrue.comments
                                    |> Rope.prependTo commentsAfterElse
                                    |> Rope.prependTo ifFalse.comments
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
                    { comments = subExpressionResult.comments
                    , syntax =
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
    Parser.oneOf
        [ Parser.map
            (\firstName ->
                \after ->
                    case after of
                        Nothing ->
                            { comments = Rope.empty, syntax = FunctionOrValue [] firstName }

                        Just ( qualificationAfter, unqualified ) ->
                            { comments = Rope.empty, syntax = FunctionOrValue (firstName :: qualificationAfter) unqualified }
            )
            Tokens.typeName
            |= maybeDotReferenceExpressionTuple
        , Parser.map (\unqualified -> { comments = Rope.empty, syntax = FunctionOrValue [] unqualified }) Tokens.functionName
        ]


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
                        { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
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
                { comments = commentsBefore |> Rope.prependTo subExpressionResult.comments
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
                                    { comments = leftExpression.comments |> Rope.prependTo commentsBefore
                                    , syntax = leftExpression.syntax
                                    }

                            Just combineExpressionResult ->
                                Parser.Loop
                                    { comments =
                                        leftExpression.comments
                                            |> Rope.prependTo commentsBefore
                                            |> Rope.prependTo combineExpressionResult.comments
                                    , syntax = combineExpressionResult.syntax
                                    }
                )
                Layout.optimisticLayout
                |= Parser.oneOf
                    [ Parser.map Just
                        (combineOneOfApply parser leftExpression.syntax)
                    , parserSucceedNothing
                    ]

        Nothing ->
            Parser.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


parserSucceedNothing : Parser (Maybe a)
parserSucceedNothing =
    Parser.succeed Nothing


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
            justAndThenOneOfAbovePrecedence0

        1 ->
            justAndThenOneOfAbovePrecedence1

        2 ->
            justAndThenOneOfAbovePrecedence2

        3 ->
            justAndThenOneOfAbovePrecedence3

        4 ->
            justAndThenOneOfAbovePrecedence4

        5 ->
            justAndThenOneOfAbovePrecedence5

        6 ->
            justAndThenOneOfAbovePrecedence6

        7 ->
            justAndThenOneOfAbovePrecedence7

        8 ->
            justAndThenOneOfAbovePrecedence8

        9 ->
            justAndThenOneOfAbovePrecedence9

        90 ->
            justAndThenOneOfAbovePrecedence90

        95 ->
            justAndThenOneOfAbovePrecedence95

        100 ->
            justAndThenOneOfAbovePrecedence100

        _ ->
            Nothing


justAndThenOneOfAbovePrecedence0 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence0 =
    Just (computeAndThenOneOfAbovePrecedence 0)


justAndThenOneOfAbovePrecedence1 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence1 =
    Just (computeAndThenOneOfAbovePrecedence 1)


justAndThenOneOfAbovePrecedence2 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence2 =
    Just (computeAndThenOneOfAbovePrecedence 2)


justAndThenOneOfAbovePrecedence3 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence3 =
    Just (computeAndThenOneOfAbovePrecedence 3)


justAndThenOneOfAbovePrecedence4 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence4 =
    Just (computeAndThenOneOfAbovePrecedence 4)


justAndThenOneOfAbovePrecedence5 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence5 =
    Just (computeAndThenOneOfAbovePrecedence 5)


justAndThenOneOfAbovePrecedence6 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence6 =
    Just (computeAndThenOneOfAbovePrecedence 6)


justAndThenOneOfAbovePrecedence7 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence7 =
    Just (computeAndThenOneOfAbovePrecedence 7)


justAndThenOneOfAbovePrecedence8 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence8 =
    Just (computeAndThenOneOfAbovePrecedence 8)


justAndThenOneOfAbovePrecedence9 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence9 =
    Just (computeAndThenOneOfAbovePrecedence 9)


justAndThenOneOfAbovePrecedence90 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence90 =
    Just (computeAndThenOneOfAbovePrecedence 90)


justAndThenOneOfAbovePrecedence95 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence95 =
    Just (computeAndThenOneOfAbovePrecedence 95)


justAndThenOneOfAbovePrecedence100 : Maybe (List (Node Expression -> Parser (WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence100 =
    Just (computeAndThenOneOfAbovePrecedence 100)


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
                (Parser.map (\e -> { comments = e.comments, syntax = apply left e.syntax })
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
                (Parser.map (\e -> { comments = e.comments, syntax = apply left e.syntax })
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
