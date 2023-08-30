module Elm.Parser.Declarations exposing (declaration, expression, letExpression)

import Combine exposing (Parser, lazy, many, maybe, modifyState, oneOf, or, sepBy1, string, succeed, withLocation)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing (Nestable(..))


declaration : Parser State (Node Declaration)
declaration =
    oneOf
        [ infixDeclaration
        , function
        , typeDefinition
        , portDeclaration
        ]


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep typeAnnotation


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            succeed (\args expr -> Node { start = (Node.range varPointer).start, end = (Node.range expr).end } (FunctionImplementation varPointer args expr))
                |> Combine.keep (many (functionArgument |> Combine.ignore (maybe Layout.layout)))
                |> Combine.ignore (string "=")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep expression

        fromParts : Node Signature -> Node FunctionImplementation -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.andThen
                    (\sig ->
                        maybe Layout.layoutStrict
                            |> Combine.continueWith (Node.parser functionName)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andThen functionImplementationFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : Node String -> Parser State Function
        functionWithoutSignature varPointer =
            functionImplementationFromVarPointer varPointer
                |> Combine.map (Function Nothing Nothing)
    in
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


function : Parser State (Node Declaration)
function =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map (\f -> Node (Expression.functionRange f) (Declaration.FunctionDeclaration f))


signature : Parser State Signature
signature =
    succeed Signature
        |> Combine.keep (Node.parser functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (string ":"))
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep typeAnnotation


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    succeed Infix
        |> Combine.ignore (Combine.fromCore (Core.keyword "infix"))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser (Combine.fromCore Core.int))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser <| Combine.parens prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser functionName)
        |> Combine.map Declaration.InfixDeclaration
        |> Node.parser


infixDirection : Parser State Infix.InfixDirection
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\_ -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\_ -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\_ -> Infix.Non)
        ]
        |> Combine.fromCore


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Combine.succeed
        (\(Node { start } _) sig ->
            Node
                { start = start, end = (Node.range sig.typeAnnotation).end }
                (Declaration.PortDeclaration sig)
        )
        |> Combine.keep (Node.parser portToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep signature


functionArgument : Parser State (Node Pattern)
functionArgument =
    pattern



-- Expressions


expressionNotApplication : Parser State (Node Expression)
expressionNotApplication =
    lazy
        (\() ->
            oneOf
                [ numberExpression
                , referenceExpression
                , ifBlockExpression
                , tupledExpression
                , recordAccessFunctionExpression
                , operatorExpression
                , letExpression
                , lambdaExpression
                , literalExpression
                , charLiteralExpression
                , recordExpression
                , glslExpression
                , listExpression
                , caseExpression
                ]
                |> Combine.andThen liftRecordAccess
        )


liftRecordAccess : Node Expression -> Parser State (Node Expression)
liftRecordAccess e =
    or
        (string "."
            |> Combine.continueWith (Node.parser functionName)
            |> Combine.andThen
                (\f ->
                    liftRecordAccess
                        (Node
                            { start = (Node.range e).start, end = (Node.range f).end }
                            (RecordAccess e f)
                        )
                )
        )
        (succeed e)


expression : Parser State (Node Expression)
expression =
    expressionNotApplication
        |> Combine.andThen
            (\first ->
                let
                    complete : Range -> List (Node Expression) -> Parser s (Node Expression)
                    complete lastExpressionRange rest =
                        case rest of
                            [] ->
                                succeed first

                            (Node _ (Operator _)) :: _ ->
                                Combine.fail "Expression should not end with an operator"

                            _ ->
                                succeed
                                    (Node
                                        { start = (Node.range first).start, end = lastExpressionRange.end }
                                        (Application (first :: List.reverse rest))
                                    )

                    promoter : Range -> List (Node Expression) -> Parser State (Node Expression)
                    promoter lastExpressionRange rest =
                        Layout.optimisticLayoutWith
                            (\() -> complete lastExpressionRange rest)
                            (\() ->
                                or
                                    (expressionNotApplication
                                        |> Combine.andThen (\next -> promoter (Node.range next) (next :: rest))
                                    )
                                    (Combine.succeed ()
                                        |> Combine.andThen (\() -> complete lastExpressionRange rest)
                                    )
                            )
                in
                case first of
                    Node _ (Operator _) ->
                        Combine.fail "Expression should not start with an operator"

                    _ ->
                        promoter (Node.range first) []
            )



-- End expression


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            modifyState (pushColumn location.column)
                |> Combine.continueWith p
                |> Combine.ignore (modifyState popIndent)
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
    Core.getChompedString (Core.multiComment start end NotNestable)
        |> Combine.fromCore
        |> Combine.map (String.dropLeft (String.length start) >> GLSLExpression)
        |> Combine.ignore (Combine.string end)
        |> Node.parser


listExpression : Parser State (Node Expression)
listExpression =
    let
        innerExpressions : Parser State Expression
        innerExpressions =
            succeed (::)
                |> Combine.keep expression
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep
                    (many
                        (string ","
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith expression
                        )
                    )
                |> Combine.ignore (string "]")
                |> Combine.map ListExpr
    in
    string "["
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ string "]" |> Combine.map (always (ListExpr []))
                , innerExpressions
                ]
            )
        |> Node.parser



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    string "{"
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ string "}" |> Combine.map (always (RecordExpr []))
                , recordContents
                ]
            )
        |> Node.parser


recordContents : Parser State Expression
recordContents =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser fname
                    , string "="
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.continueWith expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andThen
                            (\e ->
                                let
                                    fieldUpdate : Node RecordSetter
                                    fieldUpdate =
                                        Node.combine Tuple.pair fname e
                                in
                                Combine.oneOf
                                    [ string "}"
                                        |> Combine.map (always (RecordExpr [ fieldUpdate ]))
                                    , string ","
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.continueWith recordFields
                                        |> Combine.ignore (string "}")
                                        |> Combine.map (\fieldUpdates -> RecordExpr (fieldUpdate :: fieldUpdates))
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Node String -> Parser State Expression
recordUpdateSyntaxParser fname =
    string "|"
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.continueWith recordFields
        |> Combine.ignore (string "}")
        |> Combine.map (\e -> RecordUpdateExpression fname e)


recordFields : Parser State (List (Node RecordSetter))
recordFields =
    succeed (::)
        |> Combine.keep recordField
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep
            (many
                (string ","
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith recordField
                    |> Combine.ignore (maybe Layout.layout)
                )
            )


recordField : Parser State (Node RecordSetter)
recordField =
    Node.parser
        (succeed Tuple.pair
            |> Combine.keep (Node.parser functionName)
            |> Combine.ignore (maybe Layout.layout)
            |> Combine.ignore (string "=")
            |> Combine.ignore (maybe Layout.layout)
            |> Combine.keep expression
        )


literalExpression : Parser State (Node Expression)
literalExpression =
    Combine.map Literal (or multiLineStringLiteral stringLiteral)
        |> Node.parser


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Node.parser (Combine.map CharLiteral characterLiteral)



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    succeed
        (\(Node { start } _) args expr ->
            Lambda args expr
                |> LambdaExpression
                |> Node { start = start, end = (Node.range expr).end }
        )
        |> Combine.keep (Node.parser (string "\\"))
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep (sepBy1 (maybe Layout.layout) functionArgument)
        |> Combine.ignore (Layout.maybeAroundBothSides (string "->"))
        |> Combine.keep expression



-- Case Expression


caseStatement : Parser State Case
caseStatement =
    succeed Tuple.pair
        |> Combine.keep pattern
        |> Combine.keep
            (maybe (or Layout.layout Layout.layoutStrict)
                |> Combine.continueWith (string "->")
                |> Combine.continueWith (maybe Layout.layout)
                |> Combine.continueWith expression
            )


caseStatements : Parser State ( Location, Cases )
caseStatements =
    Combine.many1WithEndLocationForLastElement
        (\( _, case_ ) -> Node.range case_)
        caseStatementWithCorrectIndentation


caseStatementWithCorrectIndentation : Parser State Case
caseStatementWithCorrectIndentation =
    Combine.withState
        (\s ->
            Combine.withLocation
                (\l ->
                    if State.expectedColumn s == l.column then
                        caseStatement

                    else
                        Combine.fail "Indentation is incorrect to be a case statement"
                )
        )


caseExpression : Parser State (Node Expression)
caseExpression =
    Combine.succeed
        (\caseKeyword caseBlock_ ( end, cases ) ->
            Node { start = (Node.range caseKeyword).start, end = end }
                (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keep (Node.parser caseToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep expression
        |> Combine.ignore ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)



-- Let Expression


letBody : Parser State (List (Node LetDeclaration))
letBody =
    Combine.succeed (::)
        |> Combine.keep blockElement
        |> Combine.keep (many (blockElement |> Combine.ignore (maybe Layout.layout)))


blockElement : Parser State (Node LetDeclaration)
blockElement =
    pattern
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
letDestructuringDeclarationWithPattern pattern =
    succeed
        (\expr ->
            Node { start = (Node.range pattern).start, end = (Node.range expr).end } (LetDestructuring pattern expr)
        )
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore (string "=")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep expression


letExpression : Parser State (Node Expression)
letExpression =
    Combine.withLocation
        (\start ->
            succeed (\decls expr -> Node { start = start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression))
                |> Combine.ignore (string "let")
                |> Combine.ignore Layout.layout
                |> Combine.keep (withIndentedState letBody)
                |> Combine.ignore (oneOf [ Layout.layout, manySpaces ])
                |> Combine.ignore (string "in")
                |> Combine.keep (Layout.layout |> Combine.continueWith expression)
        )


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parser (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Combine.withLocation
        (\start ->
            ifToken
                |> Combine.continueWith
                    (succeed
                        (\condition ifTrue ifFalse ->
                            Node
                                { start = start, end = (Node.range ifFalse).end }
                                (IfBlock condition ifTrue ifFalse)
                        )
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore thenToken
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.keep
                            (elseToken
                                |> Combine.continueWith Layout.layout
                                |> Combine.continueWith expression
                            )
                    )
        )


operatorExpression : Parser State (Node Expression)
operatorExpression =
    let
        negationExpression : Parser State Expression
        negationExpression =
            Combine.map Negation
                (oneOf
                    [ referenceExpression
                    , numberExpression
                    , tupledExpression
                    ]
                    |> Combine.andThen liftRecordAccess
                )
    in
    Combine.oneOf
        [ string "-"
            |> Combine.continueWith
                (Combine.oneOf
                    [ negationExpression
                    , succeed (Operator "-") |> Combine.ignore Layout.layout
                    ]
                )
            |> Node.parser
        , Combine.map Operator infixOperatorToken
            |> Node.parser
        ]


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ( ModuleName, String ) -> Parser s Expression
        helper ( moduleNameSoFar, nameOrSegment ) =
            Combine.oneOf
                [ string "."
                    |> Combine.continueWith
                        (Combine.oneOf
                            [ Tokens.typeName
                                |> Combine.andThen (\t -> helper ( nameOrSegment :: moduleNameSoFar, t ))
                            , Tokens.functionName
                                |> Combine.map
                                    (\name ->
                                        FunctionOrValue
                                            (List.reverse (nameOrSegment :: moduleNameSoFar))
                                            name
                                    )
                            ]
                        )
                , Combine.succeed ()
                    |> Combine.map (\() -> FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment)
                ]
    in
    Combine.oneOf
        [ Tokens.typeName
            |> Combine.andThen (\t -> helper ( [], t ))
        , Tokens.functionName
            |> Combine.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parser


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Combine.map (\field -> RecordAccessFunction ("." ++ field))
        (string "."
            |> Combine.continueWith functionName
        )
        |> Node.parser


tupledExpression : Parser State (Node Expression)
tupledExpression =
    let
        asExpression : Node Expression -> List (Node Expression) -> Expression
        asExpression x xs =
            case xs of
                [] ->
                    ParenthesizedExpression x

                _ ->
                    TupledExpression (x :: xs)

        commaSep : Parser State (List (Node Expression))
        commaSep =
            many
                (string ","
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith expression
                    |> Combine.ignore (maybe Layout.layout)
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep expression
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep commaSep

        closingParen : Parser state ()
        closingParen =
            Combine.fromCore (Core.symbol ")")
    in
    Node.parser
        (Combine.fromCore (Core.symbol "(")
            |> Combine.continueWith
                (Combine.oneOf
                    [ closingParen |> Combine.map (always UnitExpr)
                    , -- Backtracking needed for record access expression
                      Combine.backtrackable
                        (prefixOperatorToken
                            |> Combine.ignore closingParen
                            |> Combine.map PrefixOperator
                        )
                    , nested |> Combine.ignore closingParen
                    ]
                )
        )
