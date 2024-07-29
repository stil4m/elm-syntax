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
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (Comments, WithComments)
import Rope


subExpressions : Parser (WithComments (Node Expression))
subExpressions =
    Parser.lazy (\() -> subExpressionsOneOf)


subExpressionsOneOf : Parser (WithComments (Node Expression))
subExpressionsOneOf =
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
            [ qualifiedReferenceExpression
            , unqualifiedReferenceExpression
            , literalExpression
            , numberExpression
            , tupledExpression
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


andThenOneOf : List ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
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


recordAccess : ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
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


functionCall : ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
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
                                , expression = ListExpr elements.syntax
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
    ListExpr []



-- recordExpression


recordExpression : Parser { comments : Comments, end : Location, expression : Expression }
recordExpression =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBefore ->
                    \maybeAfterCurly ->
                        \( endRow, endColumn ) ->
                            case maybeAfterCurly of
                                Nothing ->
                                    { comments = commentsBefore
                                    , end = { row = endRow, column = endColumn }
                                    , expression = expressionRecordEmpty
                                    }

                                Just afterCurly ->
                                    { comments = commentsBefore |> Rope.prependTo afterCurly.comments
                                    , end = { row = endRow, column = endColumn }
                                    , expression = afterCurly.syntax
                                    }
                )
                Layout.maybeLayout
            )
    )
        |= recordContents
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
                                    , end = end
                                    , expression =
                                        { args = firstArg.syntax :: secondUpArgs.syntax
                                        , expression = expressionResult.syntax
                                        }
                                            |> LambdaExpression
                                    }
                )
                Layout.maybeLayout
            )
    )
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


caseExpression : Parser { comments : Comments, end : Location, expression : Expression }
caseExpression =
    (Tokens.caseToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterCase ->
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
                Layout.layout
            )
    )
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


letExpression : Parser { comments : Comments, end : Location, expression : Expression }
letExpression =
    Parser.Extra.withIndent
        ((Tokens.letToken
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsAfterLet ->
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
                                    , end = end
                                    , expression = LetExpression { declarations = declarations.syntax, expression = expr }
                                    }
                    )
                    Layout.layout
                )
         )
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


numberExpression : Parser { comments : Comments, end : Location, expression : Expression }
numberExpression =
    Elm.Parser.Numbers.forgivingNumber
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Floatable n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Integer n
                }
        )
        (\n ->
            \( endRow, endColumn ) ->
                { comments = Rope.empty
                , end = { row = endRow, column = endColumn }
                , expression = Hex n
                }
        )
        |= Parser.getPosition


ifBlockExpression : Parser { comments : Comments, end : Location, expression : Expression }
ifBlockExpression =
    (Tokens.ifToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\condition ->
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
                                , end = end
                                , expression = IfBlock condition.syntax ifTrue.syntax ifFalse.syntax
                                }
                )
                expression
            )
    )
        |. Tokens.thenToken
        |= expression
        |. Tokens.elseToken
        |= Layout.layout
        |= expression


negationOperation : Parser { comments : Comments, end : Location, expression : Expression }
negationOperation =
    minusNotFollowedBySpace
        |> Parser.Extra.continueWith
            (Parser.map
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


qualifiedReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
qualifiedReferenceExpression =
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


unqualifiedReferenceExpression : Parser { comments : Comments, end : Location, expression : Expression }
unqualifiedReferenceExpression =
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
                        , expression = RecordAccessFunction ("." ++ field)
                        }
                )
                Tokens.functionName
                |= Parser.getPosition
            )


tupledExpression : Parser { comments : Comments, end : Location, expression : Expression }
tupledExpression =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                ((Tokens.parensEnd
                    |> Parser.Extra.continueWith
                        (Parser.map
                            (\( endRow, endColumn ) ->
                                { comments = Rope.empty
                                , end = { row = endRow, column = endColumn }
                                , expression = UnitExpr
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
            \tailPartsReverse ->
                \( endRow, endColumn ) ->
                    case tailPartsReverse.syntax of
                        [] ->
                            { comments = firstPart.comments
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
        |= ParserWithComments.manyWithoutReverse
            (Tokens.comma
                |> Parser.Extra.continueWith expression
            )
        |. Tokens.parensEnd
        |= Parser.getPosition



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
                    [ justCombineOneOfApply parser leftExpression.syntax
                    , parserSucceedNothing
                    ]

        Nothing ->
            Parser.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


parserSucceedNothing : Parser (Maybe a)
parserSucceedNothing =
    Parser.succeed Nothing


justCombineOneOfApply :
    List (Parser (arg -> WithComments arg))
    -> arg
    -> Parser (Maybe (WithComments arg))
justCombineOneOfApply possibilitiesForCurrentPrecedence leftExpression =
    Parser.oneOf
        (List.map
            (\parser -> parser |> Parser.map (\f -> Just (f leftExpression)))
            possibilitiesForCurrentPrecedence
        )


getAndThenOneOfAbovePrecedence : Int -> Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
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


justAndThenOneOfAbovePrecedence0 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence0 =
    Just (computeAndThenOneOfAbovePrecedence 0)


justAndThenOneOfAbovePrecedence1 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence1 =
    Just (computeAndThenOneOfAbovePrecedence 1)


justAndThenOneOfAbovePrecedence2 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence2 =
    Just (computeAndThenOneOfAbovePrecedence 2)


justAndThenOneOfAbovePrecedence3 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence3 =
    Just (computeAndThenOneOfAbovePrecedence 3)


justAndThenOneOfAbovePrecedence4 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence4 =
    Just (computeAndThenOneOfAbovePrecedence 4)


justAndThenOneOfAbovePrecedence5 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence5 =
    Just (computeAndThenOneOfAbovePrecedence 5)


justAndThenOneOfAbovePrecedence6 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence6 =
    Just (computeAndThenOneOfAbovePrecedence 6)


justAndThenOneOfAbovePrecedence7 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence7 =
    Just (computeAndThenOneOfAbovePrecedence 7)


justAndThenOneOfAbovePrecedence8 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence8 =
    Just (computeAndThenOneOfAbovePrecedence 8)


justAndThenOneOfAbovePrecedence9 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence9 =
    Just (computeAndThenOneOfAbovePrecedence 9)


justAndThenOneOfAbovePrecedence90 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence90 =
    Just (computeAndThenOneOfAbovePrecedence 90)


justAndThenOneOfAbovePrecedence95 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence95 =
    Just (computeAndThenOneOfAbovePrecedence 95)


justAndThenOneOfAbovePrecedence100 : Maybe (List (Parser (Node Expression -> WithComments (Node Expression))))
justAndThenOneOfAbovePrecedence100 =
    Just (computeAndThenOneOfAbovePrecedence 100)


computeAndThenOneOfAbovePrecedence : Int -> List (Parser (Node Expression -> WithComments (Node Expression)))
computeAndThenOneOfAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )


infixLeft : Int -> String -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixLeft precedence symbol =
    infixLeftHelp precedence
        (Parser.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Parser.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
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


infixLeftSubtraction : Int -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
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


infixLeftHelp : Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixLeftHelp precedence p apply =
    infixHelp precedence precedence p apply


infixLeftWithState : Int -> Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixLeftWithState precedence operator apply =
    ( precedence
    , operator
        |> Parser.Extra.continueWith
            (Parser.map (\e -> \left -> { comments = e.comments, syntax = apply left e.syntax })
                (subExpression precedence)
            )
    )


infixRightHelp : Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Parser.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Parser (Node Expression -> WithComments (Node Expression)) )
infixHelp leftPrecedence rightPrecedence operator apply =
    ( leftPrecedence
    , operator
        |> Parser.Extra.continueWith
            (Parser.map (\e -> \left -> { comments = e.comments, syntax = apply left e.syntax })
                (subExpression rightPrecedence)
            )
    )


postfix : Int -> Parser a -> (expr -> a -> expr) -> ( Int, Parser (expr -> WithComments expr) )
postfix precedence operator apply =
    ( precedence
    , Parser.map
        (\right ->
            \left ->
                { comments = Rope.empty
                , syntax = apply left right
                }
        )
        operator
    )
