module Elm.Parser.Expression exposing (expression)

import Combine exposing (Parser, Step(..))
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Expression as Expression exposing (Case, Expression(..), LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|.), (|=), Nestable(..))
import Parser.Extra


subExpressions : Parser State (Node Expression)
subExpressions =
    Combine.lazy (\() -> subExpressionsOneOf)


subExpressionsOneOf : Parser State (Node Expression)
subExpressionsOneOf =
    Combine.oneOf
        [ Combine.oneOf
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


andThenOneOf : List ( Int, Node Expression -> Parser State (Node Expression) )
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


expression : Parser State (Node Expression)
expression =
    subExpression 0


recordAccess : ( Int, Node Expression -> Parser State (Node Expression) )
recordAccess =
    postfix 100
        recordAccessParser
        (\((Node { start } _) as left) ((Node { end } _) as field) ->
            Node
                { start = start, end = end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Parser State (Node String)
recordAccessParser =
    lookBehindOneCharacter
        |> Core.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    problemRecordAccessStartingWithSpace

                else
                    dotField
            )
        |> Combine.fromCore


problemRecordAccessStartingWithSpace : Core.Parser a
problemRecordAccessStartingWithSpace =
    Core.problem "Record access can't start with a space"


dotField : Core.Parser (Node String)
dotField =
    Tokens.dot
        |> Parser.Extra.continueWith (Node.parserCore Tokens.functionName)


functionCall : ( Int, Node Expression -> Parser State (Node Expression) )
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


glslExpression : Parser State Expression
glslExpression =
    Core.mapChompedString
        (\s () -> s |> String.dropLeft glslStartLength |> GLSLExpression)
        (Core.multiComment glslStart glslEnd NotNestable)
        |. Core.symbol glslEnd
        |> Combine.fromCore


listExpression : Parser State Expression
listExpression =
    Core.map (\() -> ListExpr)
        Tokens.squareStart
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep (Combine.sepBy "," expression)
        |> Combine.ignoreEntirely Tokens.squareEnd



-- recordExpression


recordExpression : Parser State Expression
recordExpression =
    Tokens.curlyStart
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.continueWith
            (Combine.oneOf
                [ recordContents
                , Combine.succeed (RecordExpr [])
                ]
            )
        |> Combine.ignoreEntirely Tokens.curlyEnd


recordContents : Parser State Expression
recordContents =
    Node.parserCoreMap
        (\nameNode ->
            \afterName ->
                case afterName of
                    RecordUpdateExpressionAfterName fields ->
                        RecordUpdateExpression nameNode fields

                    FieldsAfterName fieldsAfterName ->
                        RecordExpr (Node.combine Tuple.pair nameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep
            (Combine.oneOf
                [ Tokens.pipe
                    |> Combine.fromCoreIgnore Layout.maybeLayout
                    |> Combine.continueWith
                        (Combine.map
                            (\firstField ->
                                \tailFields ->
                                    RecordUpdateExpressionAfterName (firstField :: tailFields)
                            )
                            recordSetterNodeWithLayout
                        )
                    |> Combine.keep recordFields
                , Tokens.equal
                    |> Combine.fromCoreIgnore Layout.maybeLayout
                    |> Combine.continueWith
                        (Combine.map
                            (\firstFieldValue ->
                                \tailFields ->
                                    FieldsAfterName { firstFieldValue = firstFieldValue, tailFields = tailFields }
                            )
                            expression
                        )
                    |> Combine.keep recordFields
                ]
            )
        |> Combine.ignore Layout.maybeLayout


type RecordFieldsOrUpdateAfterName
    = RecordUpdateExpressionAfterName (List (Node RecordSetter))
    | FieldsAfterName { firstFieldValue : Node Expression, tailFields : List (Node RecordSetter) }


recordFields : Parser State (List (Node RecordSetter))
recordFields =
    Combine.many
        (Tokens.comma
            |> Combine.fromCoreIgnore Layout.maybeLayout
            |> Combine.continueWith recordSetterNodeWithLayout
        )


recordSetterNodeWithLayout : Parser State (Node RecordSetter)
recordSetterNodeWithLayout =
    Node.parserCoreMap
        (\((Node fnNameRange _) as fnName) ->
            \expr ->
                \( endRow, endColumn ) ->
                    Node { start = fnNameRange.start, end = { row = endRow, column = endColumn } }
                        ( fnName, expr )
        )
        Tokens.functionName
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep expression
        |> Combine.ignore Layout.maybeLayout
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: remove and use (Node.range expr).end
        |> Combine.keepFromCore Core.getPosition


literalExpression : Parser State Expression
literalExpression =
    Tokens.singleOrTripleQuotedStringLiteral
        |> Combine.fromCoreMap Literal


charLiteralExpression : Parser State Expression
charLiteralExpression =
    Tokens.characterLiteral
        |> Combine.fromCoreMap CharLiteral



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    Core.map
        (\( startRow, startColumn ) ->
            \args ->
                \((Node { end } _) as expr) ->
                    { args = args, expression = expr }
                        |> LambdaExpression
                        |> Node { start = { row = startRow, column = startColumn }, end = end }
        )
        Core.getPosition
        |. Tokens.backSlash
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep (Combine.sepBy1WithState Layout.maybeLayout Patterns.pattern)
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignoreEntirely Tokens.arrowRight
        |> Combine.keep expression



-- Case Expression


caseExpression : Parser State (Node Expression)
caseExpression =
    Core.map
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
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keep expression
        |> Combine.ignore Layout.positivelyIndented
        |> Combine.ignoreEntirely Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)


caseStatements : Parser State ( Case, List Case )
caseStatements =
    Combine.map
        (\firstCase ->
            \lastToSecondCase ->
                ( firstCase
                , lastToSecondCase
                )
        )
        caseStatement
        |> Combine.keep
            (Combine.manyWithoutReverse caseStatement)


caseStatement : Parser State Case
caseStatement =
    Layout.onTopIndentation (\pattern -> \expr -> ( pattern, expr ))
        |> Combine.keep Patterns.pattern
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignoreEntirely Tokens.arrowRight
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep expression



-- Let Expression


letExpression : Parser State (Node Expression)
letExpression =
    withIndentedState
        (Core.map
            (\( startRow, startColumn ) ->
                \declarations ->
                    \((Node { end } _) as expr) ->
                        Node { start = { row = startRow, column = startColumn }, end = end }
                            (LetExpression { declarations = declarations, expression = expr })
            )
            Core.getPosition
            |. Tokens.letToken
            |> Combine.fromCoreIgnore Layout.layout
            |> Combine.keep (withIndentedState letDeclarations)
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.ignoreEntirely Tokens.inToken
        )
        |> Combine.keep expression


letDeclarations : Parser State (List (Node LetDeclaration))
letDeclarations =
    Combine.map (\head -> \tail -> head :: tail) blockElement
        |> Combine.keep (Combine.many blockElement)


blockElement : Parser State (Node LetDeclaration)
blockElement =
    Layout.onTopIndentation ()
        |> Combine.continueWith
            (Combine.oneOf
                [ letFunction
                , letDestructuringDeclaration
                ]
            )


letDestructuringDeclaration : Parser State (Node LetDeclaration)
letDestructuringDeclaration =
    Combine.map
        (\((Node { start } _) as pattern) ->
            \((Node { end } _) as expr) ->
                Node { start = start, end = end } (LetDestructuring pattern expr)
        )
        Patterns.pattern
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.keep expression


letFunction : Parser State (Node LetDeclaration)
letFunction =
    Node.parserCoreMap
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
        |> Combine.fromCoreIgnore Layout.maybeLayout
        |> Combine.keep
            (Combine.maybe
                (Tokens.colon
                    |> Combine.fromCoreIgnore Layout.maybeLayout
                    |> Combine.continueWith
                        (Combine.map
                            (\typeAnnotation ->
                                \implementationNameNode ->
                                    { implementationName = implementationNameNode
                                    , typeAnnotation = typeAnnotation
                                    }
                            )
                            TypeAnnotation.typeAnnotation
                        )
                    |> Combine.ignore Layout.layoutStrict
                    |> Combine.keepFromCore (Tokens.functionName |> Node.parserCore)
                    |> Combine.ignore Layout.maybeLayout
                )
            )
        |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore Layout.maybeLayout))
        |> Combine.ignoreEntirely Tokens.equal
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep expression
        |> Combine.flattenFromCore


numberExpression : Parser State Expression
numberExpression =
    Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex
        |> Combine.fromCore


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Core.map
        (\( startRow, startColumn ) ->
            \condition ->
                \ifTrue ->
                    \((Node { end } _) as ifFalse) ->
                        Node
                            { start = { row = startRow, column = startColumn }, end = end }
                            (IfBlock condition ifTrue ifFalse)
        )
        Core.getPosition
        |. Tokens.ifToken
        |> Combine.fromCoreKeep expression
        |> Combine.ignoreEntirely Tokens.thenToken
        |> Combine.keep expression
        |> Combine.ignoreEntirely Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep expression


negationOperation : Parser State (Node Expression)
negationOperation =
    minusNotFollowedBySpace
        |> Combine.fromCoreContinue
            (Combine.map
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


referenceExpression : Combine.Parser State Expression
referenceExpression =
    Combine.fromCoreMap
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


recordAccessFunctionExpression : Parser State Expression
recordAccessFunctionExpression =
    Tokens.dot
        |> Parser.Extra.continueWith Tokens.functionName
        |> Combine.fromCoreMap (\field -> RecordAccessFunction ("." ++ field))


tupledExpression : Parser State Expression
tupledExpression =
    Tokens.parensStart
        |> Combine.fromCoreContinue
            (Combine.oneOf
                ((Tokens.parensEnd |> Combine.fromCoreMap (\() -> UnitExpr))
                    :: -- since `-` alone  could indicate negation or prefix operator,
                       -- we check for `-)` first
                       (Core.symbol "-)" |> Combine.fromCoreMap (\() -> PrefixOperator "-"))
                    :: (tupledExpressionInnerNested |> Combine.ignoreEntirely Tokens.parensEnd)
                    -- and since prefix operators are much more rare than e.g. parenthesized
                    -- we check those later
                    :: allowedPrefixOperatorExceptMinusThenClosingParensOneOf
                )
            )


allowedPrefixOperatorExceptMinusThenClosingParensOneOf : List (Parser state Expression)
allowedPrefixOperatorExceptMinusThenClosingParensOneOf =
    Tokens.allowedOperatorTokens
        |> List.filter (\token -> token /= "-")
        |> List.map
            (\allowedOperatorToken ->
                Core.symbol (allowedOperatorToken ++ ")")
                    |> Combine.fromCoreMap (\() -> PrefixOperator allowedOperatorToken)
            )


tupledExpressionInnerCommaSep : Parser State (List (Node Expression))
tupledExpressionInnerCommaSep =
    Combine.many
        (Tokens.comma
            |> Combine.fromCoreContinue expression
        )


tupledExpressionInnerNested : Parser State Expression
tupledExpressionInnerNested =
    Combine.map asExpression
        expression
        |> Combine.keep tupledExpressionInnerCommaSep


asExpression : Node Expression -> List (Node Expression) -> Expression
asExpression x =
    \xs ->
        case xs of
            [] ->
                ParenthesizedExpression x

            _ ->
                TupledExpression (x :: xs)


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    let
        (Combine.Parser pFromState) =
            p
    in
    Combine.Parser
        (\state ->
            Core.getCol
                |> Core.andThen
                    (\column ->
                        pFromState (state |> State.pushIndent column)
                    )
                |> Core.map (\( finalState, pValue ) -> ( finalState |> State.popIndent, pValue ))
        )



---


subExpression : Int -> Parser State (Node Expression)
subExpression currentPrecedence =
    let
        parser : Node Expression -> Parser State (Step (Node Expression) (Node Expression))
        parser =
            expressionHelp currentPrecedence
    in
    optimisticLayoutSubExpressions
        |> Combine.andThen
            (\leftExpression -> Combine.loop leftExpression parser)


optimisticLayoutSubExpressions : Parser State (Node Expression)
optimisticLayoutSubExpressions =
    Layout.optimisticLayout
        |> Combine.continueWith subExpressions


expressionHelp : Int -> Node Expression -> Parser State (Step (Node Expression) (Node Expression))
expressionHelp currentPrecedence leftExpression =
    case getAndThenOneOfAbovePrecedence currentPrecedence of
        Just parser ->
            Layout.optimisticLayout
                |> Combine.continueWith
                    (Combine.maybeMap Loop
                        (Done leftExpression)
                        (combineOneOfApply parser leftExpression)
                    )

        Nothing ->
            Combine.problem ("Could not find operators related to precedence " ++ String.fromInt currentPrecedence)


combineOneOfApply :
    List (arg -> Combine.Parser State arg)
    -> arg
    -> Combine.Parser State arg
combineOneOfApply possibilitiesForCurrentPrecedence leftExpression =
    Combine.Parser <|
        \state ->
            Core.oneOf
                (List.map
                    (\parser ->
                        let
                            (Combine.Parser x) =
                                parser leftExpression
                        in
                        x state
                    )
                    possibilitiesForCurrentPrecedence
                )


getAndThenOneOfAbovePrecedence : Int -> Maybe (List (Node Expression -> Combine.Parser State (Node Expression)))
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


andThenOneOfAbovePrecedence0 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence0 =
    computeAndThenOneOfAbovePrecedence 0


andThenOneOfAbovePrecedence1 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence1 =
    computeAndThenOneOfAbovePrecedence 1


andThenOneOfAbovePrecedence2 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence2 =
    computeAndThenOneOfAbovePrecedence 2


andThenOneOfAbovePrecedence3 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence3 =
    computeAndThenOneOfAbovePrecedence 3


andThenOneOfAbovePrecedence4 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence4 =
    computeAndThenOneOfAbovePrecedence 4


andThenOneOfAbovePrecedence5 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence5 =
    computeAndThenOneOfAbovePrecedence 5


andThenOneOfAbovePrecedence6 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence6 =
    computeAndThenOneOfAbovePrecedence 6


andThenOneOfAbovePrecedence7 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence7 =
    computeAndThenOneOfAbovePrecedence 7


andThenOneOfAbovePrecedence8 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence8 =
    computeAndThenOneOfAbovePrecedence 8


andThenOneOfAbovePrecedence9 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence9 =
    computeAndThenOneOfAbovePrecedence 9


andThenOneOfAbovePrecedence90 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence90 =
    computeAndThenOneOfAbovePrecedence 90


andThenOneOfAbovePrecedence95 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence95 =
    computeAndThenOneOfAbovePrecedence 95


andThenOneOfAbovePrecedence100 : List (Node Expression -> Combine.Parser State (Node Expression))
andThenOneOfAbovePrecedence100 =
    computeAndThenOneOfAbovePrecedence 100


computeAndThenOneOfAbovePrecedence : Int -> List (Node Expression -> Combine.Parser State (Node Expression))
computeAndThenOneOfAbovePrecedence currentPrecedence =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just parser

                else
                    Nothing
            )


infixLeft : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeft precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
infixNonAssociative precedence symbol =
    infixLeftHelp precedence
        (Core.symbol symbol)
        (\((Node { start } _) as left) ((Node { end } _) as right) ->
            Node
                { start = start, end = end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> ( Int, Node Expression -> Parser State (Node Expression) )
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


infixLeftSubtraction : Int -> ( Int, Node Expression -> Parser State (Node Expression) )
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


infixLeftHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftHelp precedence p apply =
    infixHelp precedence precedence p apply


infixLeftWithState : Int -> Parser State () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftWithState precedence operator apply =
    let
        parser : Parser State (Node Expression)
        parser =
            subExpression precedence
    in
    ( precedence
    , \left ->
        operator
            |> Combine.continueWith (Combine.map (\e -> apply left e) parser)
    )


infixRightHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixHelp leftPrecedence rightPrecedence operator apply =
    let
        parser : Parser State (Node Expression)
        parser =
            subExpression rightPrecedence
    in
    ( leftPrecedence
    , \left ->
        Core.map (\() -> \e -> apply left e)
            operator
            |> Combine.fromCoreKeep parser
    )


postfix : Int -> Parser state a -> (expr -> a -> expr) -> ( Int, expr -> Parser state expr )
postfix precedence operator apply =
    ( precedence
    , \left -> Combine.map (\right -> apply left right) operator
    )
