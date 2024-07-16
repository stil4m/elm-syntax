module Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)

import Combine exposing (Parser, Step(..))
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns as Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Parser.Whitespace as Whitespace
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|.), (|=), Nestable(..))
import Parser.Extra


subExpressions : Parser State (Node Expression)
subExpressions =
    Combine.lazy
        (\() ->
            Combine.oneOf
                [ referenceExpression
                , literalExpression
                , numberExpression
                , tupledExpression
                , glslExpression
                , listExpression
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
    Core.succeed (\offset -> \source -> String.slice (offset - 1) offset source)
        |= Core.getOffset
        |= Core.getSource
        |> Core.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    Core.problem "Record access can't start with a space"

                else
                    Core.succeed identity
                        |. dot
                        |= Node.parserCore Tokens.functionName
            )
        |> Combine.fromCore


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


glslExpression : Parser State (Node Expression)
glslExpression =
    let
        start : String
        start =
            "[glsl|"

        end : String
        end =
            "|]"
    in
    Core.mapChompedString
        (\s () -> s |> String.dropLeft (String.length start) |> GLSLExpression)
        (Core.multiComment start end NotNestable)
        |. Core.symbol end
        |> Node.parserCore
        |> Combine.fromCore


listExpression : Parser State (Node Expression)
listExpression =
    Combine.succeed ListExpr
        |> Combine.ignoreEntirely squareStart
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (Combine.sepBy "," (subExpression 0))
        |> Combine.ignoreEntirely squareEnd
        |> Node.parser



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    curlyStart
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ curlyEnd
                    |> Core.map (\() -> RecordExpr [])
                    |> Combine.fromCore
                , recordContents
                ]
            )
        |> Node.parser


recordContents : Parser State Expression
recordContents =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser fname
                    , Combine.fromCore equal
                        |> Combine.continueWith (subExpression 0)
                        |> Combine.andThen
                            (\e ->
                                let
                                    fieldUpdate : Node RecordSetter
                                    fieldUpdate =
                                        Node.combine Tuple.pair fname e

                                    toRecordExpr : List (Node RecordSetter) -> Expression
                                    toRecordExpr fieldUpdates =
                                        RecordExpr (fieldUpdate :: fieldUpdates)
                                in
                                Combine.oneOf
                                    [ curlyEnd
                                        |> Core.map (\() -> toRecordExpr [])
                                        |> Combine.fromCore
                                    , Combine.succeed toRecordExpr
                                        |> Combine.ignoreEntirely comma
                                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                        |> Combine.keep recordFields
                                        |> Combine.ignoreEntirely curlyEnd
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Node String -> Parser State Expression
recordUpdateSyntaxParser fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignoreEntirely pipe
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep recordFields
        |> Combine.ignoreEntirely curlyEnd


recordFields : Parser State (List (Node RecordSetter))
recordFields =
    Combine.succeed (\first -> \rest -> first :: rest)
        |> Combine.keep recordField
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep
            (Combine.many
                (Combine.fromCore comma
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith recordField
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
            )


recordField : Parser State (Node RecordSetter)
recordField =
    Combine.succeed (\fnName -> \expr -> ( fnName, expr ))
        |> Combine.keep recordFieldWithoutValue
        |> Combine.keep (subExpression 0)
        |> Node.parser


recordFieldWithoutValue : Parser State (Node String)
recordFieldWithoutValue =
    Node.parserCore Tokens.functionName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely equal


literalExpression : Parser State (Node Expression)
literalExpression =
    Core.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Core.map Literal
        |> Node.parserCore
        |> Combine.fromCore


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Tokens.characterLiteral
        |> Core.map CharLiteral
        |> Node.parserCore
        |> Combine.fromCore



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    Combine.succeed
        (\start ->
            \args ->
                \((Node { end } _) as expr) ->
                    Lambda args expr
                        |> LambdaExpression
                        |> Node { start = start, end = end }
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely backSlash
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (Combine.sepBy1WithState (Combine.maybeIgnore Layout.layout) Patterns.pattern)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely arrowRight
        |> Combine.keep (subExpression 0)



-- Case Expression


caseExpression : Parser State (Node Expression)
caseExpression =
    Combine.succeed
        (\start ->
            \caseBlock_ ->
                \( end, cases ) ->
                    Node { start = start, end = end }
                        (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.caseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (subExpression 0)
        |> Combine.ignore Layout.positivelyIndented
        |> Combine.ignoreEntirely Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)


caseStatements : Parser State ( Location, Cases )
caseStatements =
    Combine.many1WithEndLocationForLastElement (\( _, Node range _ ) -> range) caseStatement


caseStatement : Parser State Case
caseStatement =
    Combine.succeed (\pattern -> \expr -> ( pattern, expr ))
        |> Combine.ignore Layout.onTopIndentation
        |> Combine.keep Patterns.pattern
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely arrowRight
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep (subExpression 0)



-- Let Expression


letExpression : Parser State (Node Expression)
letExpression =
    withIndentedState
        (Combine.succeed
            (\start ->
                \declarations ->
                    \((Node { end } _) as expr) ->
                        Node { start = start, end = end }
                            (LetExpression (LetBlock declarations expr))
            )
            |> Combine.keepFromCore Parser.Extra.location
            |> Combine.ignoreEntirely Tokens.letToken
            |> Combine.ignore Layout.layout
            |> Combine.keep (withIndentedState letDeclarations)
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.ignoreEntirely Tokens.inToken
        )
        |> Combine.keep (subExpression 0)


letDeclarations : Parser State (List (Node LetDeclaration))
letDeclarations =
    Combine.many1 blockElement


blockElement : Parser State (Node LetDeclaration)
blockElement =
    Layout.onTopIndentation
        |> Combine.continueWith Patterns.pattern
        |> Combine.andThen
            (\(Node r p) ->
                case p of
                    Pattern.VarPattern v ->
                        functionWithNameNode (Node r v)
                            |> Combine.map (\fn -> Node (Expression.functionRange fn) (LetFunction fn))

                    _ ->
                        letDestructuringDeclarationWithPattern (Node r p)
            )


letDestructuringDeclarationWithPattern : Node Pattern -> Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern ((Node { start } _) as pattern) =
    Combine.succeed
        (\((Node { end } _) as expr) ->
            Node { start = start, end = end } (LetDestructuring pattern expr)
        )
        |> Combine.ignoreEntirely equal
        |> Combine.keep (subExpression 0)


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parserCore (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)
        |> Combine.fromCore


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Combine.succeed
        (\start ->
            \condition ->
                \ifTrue ->
                    \((Node { end } _) as ifFalse) ->
                        Node
                            { start = start, end = end }
                            (IfBlock condition ifTrue ifFalse)
        )
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.ignoreEntirely Tokens.ifToken
        |> Combine.keep (subExpression 0)
        |> Combine.ignoreEntirely Tokens.thenToken
        |> Combine.keep (subExpression 0)
        |> Combine.ignoreEntirely Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (subExpression 0)


negationOperation : Parser State (Node Expression)
negationOperation =
    Combine.succeed
        (\((Node { start, end } _) as subExpr) ->
            Node
                { start = { row = start.row, column = start.column - 1 }, end = end }
                (Negation subExpr)
        )
        |> Combine.ignoreEntirely minusNotFollowedBySpace
        |> Combine.keep (subExpression 95)


minusNotFollowedBySpace : Core.Parser ()
minusNotFollowedBySpace =
    Core.succeed identity
        |. Core.backtrackable minus
        |= Core.oneOf
            [ Core.map (always True) (Core.backtrackable Whitespace.realNewLine)
            , Core.map (always True) (Core.backtrackable (Core.symbol " "))
            , Core.succeed False
            ]
        |> Core.andThen
            (\isSpaceOrComment ->
                if isSpaceOrComment then
                    Core.problem "negation sign cannot be followed by a space"

                else
                    Core.commit ()
            )


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ModuleName -> String -> Core.Parser Expression
        helper moduleNameSoFar nameOrSegment =
            Core.oneOf
                [ Core.succeed identity
                    |. dot
                    |= Core.oneOf
                        [ Tokens.typeName
                            |> Core.andThen (\t -> helper (nameOrSegment :: moduleNameSoFar) t)
                        , Tokens.functionName
                            |> Core.map
                                (\name ->
                                    FunctionOrValue
                                        (List.reverse (nameOrSegment :: moduleNameSoFar))
                                        name
                                )
                        ]
                , Core.succeed ()
                    |> Core.map (\() -> FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment)
                ]
    in
    Core.oneOf
        [ Tokens.typeName
            |> Core.andThen (\t -> helper [] t)
        , Tokens.functionName
            |> Core.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parserCore
        |> Combine.fromCore


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Core.succeed (\field -> RecordAccessFunction ("." ++ field))
        |. dot
        |= Tokens.functionName
        |> Node.parserFromCore


tupledExpression : Parser State (Node Expression)
tupledExpression =
    let
        commaSep : Parser State (List (Node Expression))
        commaSep =
            Combine.many
                (comma
                    |> Combine.continueFromCore (subExpression 0)
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.keep (subExpression 0)
                |> Combine.keep commaSep
    in
    parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ parensEnd |> Core.map (always UnitExpr) |> Combine.fromCore
                , closingPrefixOperator
                , nested |> Combine.ignoreEntirely parensEnd
                ]
            )
        |> Node.parser


closingPrefixOperator : Parser state Expression
closingPrefixOperator =
    Core.backtrackable Tokens.prefixOperatorToken
        |. Core.symbol ")"
        |. Core.commit ()
        |> Core.map PrefixOperator
        |> Combine.fromCore


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
    Combine.withLocation
        (\location ->
            Combine.modifyState (State.pushIndent location.column)
                |> Combine.continueWith p
                |> Combine.ignore (Combine.modifyState State.popIndent)
        )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


functionWithSignature : Node String -> Parser State Function
functionWithSignature varPointer =
    functionSignatureFromVarPointer varPointer
        |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
        |> Combine.andThen
            (\sig ->
                Node.parserFromCore Tokens.functionName
                    |> Combine.andThen (\fnName -> failIfDifferentFrom varPointer fnName)
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen (\newPointer -> functionImplementationFromVarPointer newPointer)
                    |> Combine.map (\decl -> fromParts sig decl)
            )


functionWithoutSignature : Node String -> Parser State Function
functionWithoutSignature varPointer =
    functionImplementationFromVarPointer varPointer
        |> Combine.map (\decl -> Function Nothing Nothing decl)


functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
functionImplementationFromVarPointer ((Node { start } _) as varPointer) =
    Combine.succeed
        (\args ->
            \((Node { end } _) as expr) ->
                Node { start = start, end = end }
                    (FunctionImplementation varPointer args expr)
        )
        |> Combine.keep (Combine.many (Patterns.pattern |> Combine.ignore (Combine.maybeIgnore Layout.layout)))
        |> Combine.ignoreEntirely equal
        |> Combine.keep (subExpression 0)


fromParts : Node Signature -> Node FunctionImplementation -> Function
fromParts sig decl =
    { documentation = Nothing
    , signature = Just sig
    , declaration = decl
    }


failIfDifferentFrom : Node String -> Node String -> Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.problem <| "Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    Combine.succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignoreEntirely colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep TypeAnnotation.typeAnnotation


minus : Core.Parser ()
minus =
    Core.symbol "-"


minusSymbols : Core.Parser ()
minusSymbols =
    Core.oneOf
        [ Core.symbol "- "
        , Core.symbol "-\n"
        , Core.symbol "-\u{000D}"
        ]


dot : Core.Parser ()
dot =
    Core.symbol "."


squareStart : Core.Parser ()
squareStart =
    Core.symbol "["


squareEnd : Core.Parser ()
squareEnd =
    Core.symbol "]"


curlyStart : Core.Parser ()
curlyStart =
    Core.symbol "{"


curlyEnd : Core.Parser ()
curlyEnd =
    Core.symbol "}"


pipe : Core.Parser ()
pipe =
    Core.symbol "|"


backSlash : Core.Parser ()
backSlash =
    Core.symbol "\\"


arrowRight : Core.Parser ()
arrowRight =
    Core.symbol "->"


equal : Core.Parser ()
equal =
    Core.symbol "="


comma : Core.Parser ()
comma =
    Core.symbol ","


parensStart : Core.Parser ()
parensStart =
    Core.symbol "("


parensEnd : Core.Parser ()
parensEnd =
    Core.symbol ")"


colon : Core.Parser ()
colon =
    Core.symbol ":"



---


subExpression : Int -> Parser State (Node Expression)
subExpression =
    let
        spacesAndOneOf : Parser State (Node Expression)
        spacesAndOneOf =
            Layout.optimisticLayout
                |> Combine.continueWith subExpressions
    in
    \currentPrecedence ->
        spacesAndOneOf
            |> Combine.andThen
                (\leftExpression -> Combine.loop leftExpression (\expr -> expressionHelp currentPrecedence expr))


expressionHelp : Int -> Node Expression -> Parser State (Step (Node Expression) (Node Expression))
expressionHelp currentPrecedence leftExpression =
    Layout.optimisticLayout
        |> Combine.continueWith
            (Combine.oneOf
                [ operation currentPrecedence leftExpression
                    |> Combine.map Loop
                , Combine.succeed (Done leftExpression)
                ]
            )


operation : Int -> Node Expression -> Parser State (Node Expression)
operation currentPrecedence leftExpression =
    andThenOneOf
        |> List.filterMap
            (\( precedence, parser ) ->
                if precedence > currentPrecedence then
                    Just (parser leftExpression)

                else
                    Nothing
            )
        |> Combine.oneOf


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


infixLeftSubtraction : Int -> ( Int, Node Expression -> Parser State (Node Expression) )
infixLeftSubtraction precedence =
    infixLeftHelp precedence
        (Core.succeed (\offset -> \source -> String.slice (offset - 1) offset source)
            |= Core.getOffset
            |= Core.getSource
            |> Core.andThen
                (\c ->
                    -- 'a-b', 'a - b' and 'a- b' are subtractions, but 'a -b' is an application on a negation
                    if c == " " || c == "\n" || c == "\u{000D}" then
                        minusSymbols

                    else
                        minus
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
    ( precedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignore operator
            |> Combine.keep (subExpression precedence)
    )


infixRightHelp : Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixRightHelp precedence p apply =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply


infixHelp : Int -> Int -> Core.Parser () -> (Node Expression -> Node Expression -> Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
infixHelp leftPrecedence rightPrecedence operator apply =
    ( leftPrecedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignoreEntirely operator
            |> Combine.keep (subExpression rightPrecedence)
    )


postfix : Int -> Parser state a -> (expr -> a -> expr) -> ( Int, expr -> Parser state expr )
postfix precedence operator apply =
    ( precedence
    , \left -> Combine.map (\right -> apply left right) operator
    )
