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
    CustomParser.map2
        (\start expressionAndEnd ->
            { comments = expressionAndEnd.comments
            , syntax =
                Node
                    { start = start
                    , end = expressionAndEnd.end
                    }
                    expressionAndEnd.expression
            }
        )
        CustomParser.getPosition
        (CustomParser.oneOf
            [ qualifiedOrVariantOrRecordConstructorReferenceExpression
            , unqualifiedFunctionReferenceExpression
            , literalExpression
            , numberExpression
            , tupledExpression
            , Tokens.squareStart |> CustomParser.Extra.continueWith expressionAfterOpeningSquareBracket
            , recordExpression
            , caseExpression
            , lambdaExpression
            , letExpression
            , ifBlockExpression
            , recordAccessFunctionExpression
            , negationOperation
            , charLiteralExpression
            ]
        )


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
    subExpressionMap identity abovePrecedence0


recordAccess : ( Int, Parser (WithComments ExtensionRight) )
recordAccess =
    postfix 100
        recordAccessParser
        ExtendRightByRecordAccess


recordAccessParser : Parser (Node String)
recordAccessParser =
    lookBehindOneCharacter
        |> CustomParser.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    problemRecordAccessStartingWithSpace

                else
                    dotField
            )


problemRecordAccessStartingWithSpace : CustomParser.Parser a
problemRecordAccessStartingWithSpace =
    CustomParser.problem "Record access can't start with a space"


dotField : CustomParser.Parser (Node String)
dotField =
    CustomParser.map3
        (\() nameStart name ->
            Node.singleLineStringFrom nameStart
                name
        )
        Tokens.dot
        CustomParser.getPosition
        Tokens.functionName


functionCall : ( Int, Parser (WithComments ExtensionRight) )
functionCall =
    infixHelp 90
        (CustomParser.lazy (\() -> abovePrecedence90))
        (Layout.positivelyIndented ())
        ExtendRightByApplication


glslExpressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
glslExpressionAfterOpeningSquareBracket =
    CustomParser.map3
        (\() s end ->
            { comments = Rope.empty
            , -- TODO for v8: don't include glslEnd in range
              end = { row = end.row, column = end.column + glslEndSymbolLength }
            , expression = GLSLExpression s
            }
        )
        (CustomParser.symbol "glsl|")
        (CustomParser.Advanced.loop "" untilGlslEnd)
        CustomParser.getPosition


glslEndSymbolLength : Int
glslEndSymbolLength =
    String.length glslEndSymbol


glslEndSymbol : String
glslEndSymbol =
    "|]"


untilGlslEnd : String -> Parser (CustomParser.Advanced.Step String String)
untilGlslEnd soFar =
    CustomParser.oneOf
        [ CustomParser.map (\() -> CustomParser.Advanced.Done soFar)
            (CustomParser.symbol glslEndSymbol)
        , CustomParser.mapChompedString
            (\beforeVerticalBar () ->
                CustomParser.Advanced.Loop (soFar ++ beforeVerticalBar)
            )
            (CustomParser.chompIf (\c -> c /= '|')
                |> CustomParser.ignore
                    (CustomParser.chompWhile (\c -> c /= '|'))
            )
        , CustomParser.map
            (\() -> CustomParser.Advanced.Loop (soFar ++ "|"))
            (CustomParser.symbol "|")
        ]


expressionAfterOpeningSquareBracket : Parser { comments : Comments, end : Location, expression : Expression }
expressionAfterOpeningSquareBracket =
    CustomParser.oneOf
        [ glslExpressionAfterOpeningSquareBracket
        , CustomParser.map3
            (\commentsBefore maybeElements end ->
                case maybeElements of
                    Nothing ->
                        { comments = commentsBefore
                        , end = end
                        , expression = expressionListEmpty
                        }

                    Just elements ->
                        { comments = commentsBefore |> Rope.prependTo elements.comments
                        , end = end
                        , expression = ListExpr elements.syntax
                        }
            )
            Layout.maybeLayout
            (CustomParser.oneOf
                [ CustomParser.map (\() -> Nothing) Tokens.squareEnd
                , CustomParser.map4
                    (\head commentsAfterHead tail () ->
                        Just
                            { comments =
                                head.comments
                                    |> Rope.prependTo commentsAfterHead
                                    |> Rope.prependTo tail.comments
                            , syntax = head.syntax :: tail.syntax
                            }
                    )
                    expression
                    Layout.maybeLayout
                    (ParserWithComments.many
                        (Tokens.comma
                            |> CustomParser.Extra.continueWith (Layout.maybeAroundBothSides expression)
                        )
                    )
                    Tokens.squareEnd
                ]
            )
            CustomParser.getPosition
        ]


expressionListEmpty : Expression
expressionListEmpty =
    ListExpr []



-- recordExpression


recordExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordExpression =
    CustomParser.map4
        (\() commentsBefore afterCurly end ->
            { comments =
                commentsBefore
                    |> Rope.prependTo afterCurly.comments
            , end = end
            , expression = afterCurly.syntax
            }
        )
        Tokens.curlyStart
        Layout.maybeLayout
        recordContentsCurlyEnd
        CustomParser.getPosition


expressionRecordEmptyWithComments : WithComments Expression
expressionRecordEmptyWithComments =
    { comments = Rope.empty, syntax = RecordExpr [] }


recordContentsCurlyEnd : Parser (WithComments Expression)
recordContentsCurlyEnd =
    CustomParser.oneOf
        [ CustomParser.map6
            (\nameStart name commentsAfterFunctionName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                let
                    nameNode : Node String
                    nameNode =
                        Node.singleLineStringFrom nameStart
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
            CustomParser.getPosition
            Tokens.functionName
            Layout.maybeLayout
            (CustomParser.oneOf
                [ CustomParser.map3
                    (\() commentsBefore setterResult ->
                        { comments = commentsBefore |> Rope.prependTo setterResult.comments
                        , syntax = RecordUpdateFirstSetter setterResult.syntax
                        }
                    )
                    Tokens.pipe
                    Layout.maybeLayout
                    recordSetterNodeWithLayout
                , CustomParser.map4
                    (\() commentsBefore expressionResult commentsAfter ->
                        { comments =
                            commentsBefore
                                |> Rope.prependTo expressionResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = FieldsFirstValue expressionResult.syntax
                        }
                    )
                    Tokens.equal
                    Layout.maybeLayout
                    expression
                    Layout.maybeLayout
                ]
            )
            recordFields
            (Layout.maybeLayoutUntilIgnored CustomParser.token "}")
        , CustomParser.map (\() -> expressionRecordEmptyWithComments) Tokens.curlyEnd
        ]


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (Node RecordSetter)
    | FieldsFirstValue (Node Expression)


recordFields : Parser (WithComments (List (Node RecordSetter)))
recordFields =
    ParserWithComments.many
        (CustomParser.map3
            (\() commentsBefore setterResult ->
                { comments = commentsBefore |> Rope.prependTo setterResult.comments
                , syntax = setterResult.syntax
                }
            )
            Tokens.comma
            Layout.maybeLayout
            recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser (WithComments (Node RecordSetter))
recordSetterNodeWithLayout =
    CustomParser.map7
        (\nameStart name commentsAfterFunctionName commentsAfterEquals expressionResult commentsAfterExpression end ->
            { comments =
                commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo expressionResult.comments
                    |> Rope.prependTo commentsAfterExpression
            , syntax =
                Node { start = nameStart, end = end }
                    ( Node.singleLineStringFrom nameStart
                        name
                    , expressionResult.syntax
                    )
            }
        )
        CustomParser.getPosition
        Tokens.functionName
        (Layout.maybeLayoutUntilIgnored CustomParser.token "=")
        Layout.maybeLayout
        expression
        Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        CustomParser.getPosition


literalExpression : Parser { comments : Comments, end : Location, expression : Expression }
literalExpression =
    CustomParser.map2
        (\string end ->
            { comments = Rope.empty
            , end = end
            , expression = Literal string
            }
        )
        Tokens.singleOrTripleQuotedStringLiteral
        CustomParser.getPosition


charLiteralExpression : Parser { comments : Comments, end : Location, expression : Expression }
charLiteralExpression =
    CustomParser.map2
        (\char end ->
            { comments = Rope.empty
            , end = end
            , expression = CharLiteral char
            }
        )
        Tokens.characterLiteral
        CustomParser.getPosition



-- lambda


lambdaExpression : Parser { comments : Comments, end : Location, expression : Expression }
lambdaExpression =
    CustomParser.map6
        (\() commentsAfterBackslash firstArg commentsAfterFirstArg secondUpArgs expressionResult ->
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
        Layout.maybeLayout
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



-- Case Expression


caseExpression : Parser { comments : Comments, end : Location, expression : Expression }
caseExpression =
    CustomParser.map6
        (\() commentsAfterCase casedExpressionResult commentsBeforeOf commentsAfterOf casesResult ->
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
        Layout.maybeLayout
        expression
        (Layout.maybeLayoutUntilIgnored CustomParser.keyword "of")
        Layout.maybeLayout
        (CustomParser.Extra.withIndent caseStatements)


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
        (Layout.maybeLayoutUntilIgnored CustomParser.token "->")
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
        (Layout.onTopIndentation ())
        Patterns.pattern
        (Layout.maybeLayoutUntilIgnored CustomParser.token "->")
        Layout.maybeLayout
        expression



-- Let Expression


letExpression : Parser { comments : Comments, end : Location, expression : Expression }
letExpression =
    CustomParser.Extra.withIndent
        (CustomParser.map3
            (\() commentsAfterLet declarations ->
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
            Layout.maybeLayout
            (CustomParser.Extra.withIndent letDeclarationsIn)
        )
        -- check that the `in` token used as the end parser in letDeclarationsIn is indented correctly
        |> CustomParser.ignore (Layout.positivelyIndentedPlus 2)
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression


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
        |> CustomParser.keep
            (CustomParser.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )
        |> CustomParser.keep Layout.optimisticLayout
        |> CustomParser.keep (ParserWithComments.until Tokens.inToken blockElement)


blockElement : Parser (WithComments (Node LetDeclaration))
blockElement =
    Layout.onTopIndentation
        (\letDeclarationResult ->
            \commentsAfter ->
                { comments = letDeclarationResult.comments |> Rope.prependTo commentsAfter
                , syntax = letDeclarationResult.syntax
                }
        )
        |> CustomParser.keep
            (CustomParser.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )
        |> CustomParser.keep Layout.optimisticLayout


letDestructuringDeclaration : Parser (WithComments (Node LetDeclaration))
letDestructuringDeclaration =
    CustomParser.map3
        (\pattern () expressionResult ->
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
        Tokens.equal
        expression


letFunction : Parser (WithComments (Node LetDeclaration))
letFunction =
    CustomParser.map7
        (\startNameStart startName commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
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

                startNameNode : Node String
                startNameNode =
                    Node.singleLineStringFrom startNameStart
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
        CustomParser.getPosition
        Tokens.functionName
        Layout.maybeLayout
        (CustomParser.oneOf
            [ CustomParser.map7
                (\() commentsBeforeTypeAnnotation typeAnnotationResult commentsAfterTypeAnnotation implementationNameStart implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> Rope.prependTo typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfterTypeAnnotation
                                |> Rope.prependTo afterImplementationName
                        , implementationName =
                            Node.singleLineStringFrom implementationNameStart
                                implementationName
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                Tokens.colon
                Layout.maybeLayout
                TypeAnnotation.typeAnnotation
                Layout.layoutStrict
                CustomParser.getPosition
                Tokens.functionName
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
            (\patternResult ->
                \commentsAfterPattern ->
                    { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                    , syntax = patternResult.syntax
                    }
            )
            Patterns.pattern
            Layout.maybeLayout
        )


numberExpression : Parser { comments : Comments, end : Location, expression : Expression }
numberExpression =
    Elm.Parser.Numbers.forgivingNumber
        (\n ->
            \end ->
                { comments = Rope.empty
                , end = end
                , expression = Floatable n
                }
        )
        (\n ->
            \end ->
                { comments = Rope.empty
                , end = end
                , expression = Integer n
                }
        )
        (\n ->
            \end ->
                { comments = Rope.empty
                , end = end
                , expression = Hex n
                }
        )
        |> CustomParser.keep CustomParser.getPosition


ifBlockExpression : Parser { comments : Comments, end : Location, expression : Expression }
ifBlockExpression =
    CustomParser.map
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
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.keyword "then")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.keyword "else")
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep expression


negationOperation : Parser { comments : Comments, end : Location, expression : Expression }
negationOperation =
    minusNotFollowedBySpace
        |> CustomParser.Extra.continueWith
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


minusNotFollowedBySpace : CustomParser.Parser ()
minusNotFollowedBySpace =
    Tokens.minus
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                [ CustomParser.chompIf (\next -> next == '\u{000D}' || next == '\n' || next == ' ')
                    |> CustomParser.Extra.continueWith (CustomParser.problem "negation sign cannot be followed by a space")
                , CustomParser.succeed ()
                ]
            )
        |> CustomParser.backtrackable


qualifiedOrVariantOrRecordConstructorReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
qualifiedOrVariantOrRecordConstructorReferenceExpression =
    CustomParser.map3
        (\firstName after end ->
            { comments = Rope.empty
            , end = end
            , expression =
                case after of
                    Nothing ->
                        FunctionOrValue [] firstName

                    Just ( qualificationAfter, unqualified ) ->
                        FunctionOrValue (firstName :: qualificationAfter) unqualified
            }
        )
        Tokens.typeName
        maybeDotReferenceExpressionTuple
        CustomParser.getPosition


unqualifiedFunctionReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
unqualifiedFunctionReferenceExpression =
    CustomParser.map2
        (\unqualified end ->
            { comments = Rope.empty
            , end = end
            , expression = FunctionOrValue [] unqualified
            }
        )
        Tokens.functionName
        CustomParser.getPosition


maybeDotReferenceExpressionTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    CustomParser.oneOf
        [ Tokens.dot
            |> CustomParser.Extra.continueWith
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


recordAccessFunctionExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordAccessFunctionExpression =
    CustomParser.map3
        (\() field end ->
            { comments = Rope.empty
            , end = end
            , expression = RecordAccessFunction ("." ++ field)
            }
        )
        Tokens.dot
        Tokens.functionName
        CustomParser.getPosition


tupledExpression : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpression =
    Tokens.parensStart
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                (CustomParser.map2
                    (\() end ->
                        { comments = Rope.empty
                        , end = end
                        , expression = UnitExpr
                        }
                    )
                    Tokens.parensEnd
                    CustomParser.getPosition
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       CustomParser.map2
                        (\() end ->
                            { comments = Rope.empty
                            , end = end
                            , expression = expressionPrefixOperatorMinus
                            }
                        )
                        (CustomParser.symbol "-)")
                        CustomParser.getPosition
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
                CustomParser.map2
                    (\() end ->
                        { comments = Rope.empty
                        , end = end
                        , expression = PrefixOperator allowedOperatorToken
                        }
                    )
                    (CustomParser.symbol (allowedOperatorToken ++ ")"))
                    CustomParser.getPosition
            )


tupledExpressionInnerAfterOpeningParens : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpressionInnerAfterOpeningParens =
    CustomParser.map4
        (\firstPart commentsAfterFirstPart tailPartsReverse end ->
            case tailPartsReverse.syntax of
                [] ->
                    { comments = firstPart.comments |> Rope.prependTo commentsAfterFirstPart
                    , end = end
                    , expression = ParenthesizedExpression firstPart.syntax
                    }

                _ ->
                    { comments = firstPart.comments |> Rope.prependTo tailPartsReverse.comments
                    , end = end
                    , expression = TupledExpression (firstPart.syntax :: List.reverse tailPartsReverse.syntax)
                    }
        )
        expression
        Layout.maybeLayout
        (ParserWithComments.untilWithoutReverse
            Tokens.parensEnd
            (CustomParser.map4
                (\() commentsBefore partResult commentsAfter ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo partResult.comments
                            |> Rope.prependTo commentsAfter
                    , syntax = partResult.syntax
                    }
                )
                Tokens.comma
                Layout.maybeLayout
                expression
                Layout.maybeLayout
            )
        )
        CustomParser.getPosition



---


subExpressionMap :
    (WithComments (Node Expression) -> a)
    -> Parser (WithComments ExtensionRight)
    -> Parser a
subExpressionMap toExtensionRightWith aboveCurrentPrecedenceLayout =
    let
        step :
            WithComments (Node Expression)
            -> Parser (CustomParser.Advanced.Step (WithComments (Node Expression)) a)
        step leftExpressionResult =
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
                    (CustomParser.Advanced.Done (toExtensionRightWith leftExpressionResult))
                ]
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
        (CustomParser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Left, expression = right }
        )


infixNonAssociative : Int -> Parser (WithComments ExtensionRight) -> String -> ( Int, Parser (WithComments ExtensionRight) )
infixNonAssociative precedence possibilitiesForPrecedence symbol =
    infixHelp precedence
        possibilitiesForPrecedence
        (CustomParser.symbol symbol)
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
        (CustomParser.symbol symbol)
        (\right ->
            ExtendRightByOperation { symbol = symbol, direction = Infix.Right, expression = right }
        )


lookBehindOneCharacter : CustomParser.Parser String
lookBehindOneCharacter =
    CustomParser.map2 (\offset source -> String.slice (offset - 1) offset source)
        CustomParser.getOffset
        CustomParser.getSource


infixLeftSubtraction : Int -> Parser (WithComments ExtensionRight) -> ( Int, Parser (WithComments ExtensionRight) )
infixLeftSubtraction precedence possibilitiesForPrecedence =
    infixHelp precedence
        possibilitiesForPrecedence
        (lookBehindOneCharacter
            |> CustomParser.andThen
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
    -> CustomParser.Parser ()
    -> (Node Expression -> ExtensionRight)
    -> ( Int, Parser (WithComments ExtensionRight) )
infixHelp leftPrecedence rightPrecedence operator apply =
    ( leftPrecedence
    , operator
        |> CustomParser.Extra.continueWith
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
    , CustomParser.map
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
