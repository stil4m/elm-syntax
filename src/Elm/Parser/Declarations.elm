module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionArgument, functionSignature, letBlock, letBody, letExpression, signature)

import Combine exposing (Parser, choice, lazy, many, maybe, modifyState, or, sepBy1, string, succeed, withLocation)
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.Ranges as Ranges
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range as Range
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing (Nestable(..))


declaration : Parser State (Node Declaration)
declaration =
    lazy
        (\() ->
            choice
                [ infixDeclaration
                , function
                , typeDefinition
                    |> Combine.map
                        (\v ->
                            case v of
                                Typings.DefinedType r t ->
                                    Node r (CustomTypeDeclaration t)

                                Typings.DefinedAlias r a ->
                                    Node r (AliasDeclaration a)
                        )
                , portDeclaration
                , destructuringDeclaration
                ]
        )


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andMap typeAnnotation


functionSignature : Parser State (Node Signature)
functionSignature =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionSignatureFromVarPointer


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            succeed (\args expr -> Node (Range.combine [ Node.range varPointer, Node.range expr ]) (FunctionImplementation varPointer args expr))
                |> Combine.andMap (many (functionArgument |> Combine.ignore (maybe Layout.layout)))
                |> Combine.ignore (string "=")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andMap expression

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
    Combine.choice
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


function : Parser State (Node Declaration)
function =
    lazy
        (\() ->
            Node.parser functionName
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andThen functionWithNameNode
                |> Combine.map (\f -> Node (Expression.functionRange f) (FunctionDeclaration f))
        )


signature : Parser State Signature
signature =
    succeed Signature
        |> Combine.andMap (Node.parser functionName)
        |> Combine.andMap (Layout.maybeAroundBothSides (string ":") |> Combine.continueWith (maybe Layout.layout) |> Combine.continueWith typeAnnotation)


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            Infix.infixDefinition
                |> Combine.map (\inf -> Node (Range.combine [ current, Node.range inf.function ]) (InfixDeclaration inf))
        )


destructuringDeclaration : Parser State (Node Declaration)
destructuringDeclaration =
    lazy
        (\() ->
            succeed
                (\x y -> Node.combine Destructuring x y)
                |> Combine.andMap pattern
                |> Combine.ignore (string "=")
                |> Combine.ignore Layout.layout
                |> Combine.andMap expression
        )


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            portToken
                |> Combine.ignore Layout.layout
                |> Combine.continueWith signature
                |> Combine.map (\sig -> Node (Range.combine [ current, (\(Node r _) -> r) sig.typeAnnotation ]) (PortDeclaration sig))
        )


functionArgument : Parser State (Node Pattern)
functionArgument =
    pattern



-- Expressions


expressionNotApplication : Parser State (Node Expression)
expressionNotApplication =
    lazy
        (\() ->
            choice
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
    lazy
        (\() ->
            or
                (string "."
                    |> Combine.continueWith (Node.parser functionName)
                    |> Combine.map (\f -> Node.combine RecordAccess e f)
                    |> Combine.andThen liftRecordAccess
                )
                (succeed e)
        )


expression : Parser State (Node Expression)
expression =
    lazy
        (\() ->
            expressionNotApplication
                |> Combine.andThen
                    (\first ->
                        let
                            complete rest =
                                case rest of
                                    [] ->
                                        succeed first

                                    (Node _ (Operator _)) :: _ ->
                                        Combine.fail "Expression should not end with an operator"

                                    _ ->
                                        succeed
                                            (Node
                                                (Range.combine (Node.range first :: List.map Node.range rest))
                                                (Application (first :: List.reverse rest))
                                            )

                            promoter rest =
                                Layout.optimisticLayoutWith
                                    (\() -> complete rest)
                                    (\() ->
                                        or
                                            (expressionNotApplication
                                                |> Combine.andThen (\next -> promoter (next :: rest))
                                            )
                                            (complete rest)
                                    )
                        in
                        case first of
                            Node _ (Operator _) ->
                                Combine.fail "Expression should not start with an operator"

                            _ ->
                                promoter []
                    )
        )



-- End expression


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            (modifyState (pushColumn location.column) |> Combine.continueWith p)
                |> Combine.ignore (modifyState popIndent)
        )


glslExpression : Parser State (Node Expression)
glslExpression =
    let
        start =
            "[glsl|"

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
    lazy
        (\() ->
            let
                innerExpressions =
                    succeed (::)
                        |> Combine.andMap expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap (many (string "," |> Combine.ignore (maybe Layout.layout) |> Combine.continueWith expression))
                        |> Combine.map ListExpr
            in
            string "["
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.continueWith
                    (Combine.choice
                        [ string "]" |> Combine.map (always (ListExpr []))
                        , innerExpressions |> Combine.ignore (string "]")
                        ]
                    )
                |> Node.parser
        )



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    lazy
        (\() ->
            let
                recordField : Parser State (Node RecordSetter)
                recordField =
                    Node.parser
                        (succeed Tuple.pair
                            |> Combine.andMap (Node.parser functionName)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.ignore (string "=")
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andMap expression
                        )

                recordFields : Parser State (List (Node RecordSetter))
                recordFields =
                    succeed (::)
                        |> Combine.andMap recordField
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap
                            (many
                                (string ","
                                    |> Combine.ignore (maybe Layout.layout)
                                    |> Combine.continueWith recordField
                                    |> Combine.ignore (maybe Layout.layout)
                                )
                            )

                recordUpdateSyntaxParser : Node String -> Parser State Expression
                recordUpdateSyntaxParser fname =
                    string "|"
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.continueWith recordFields
                        |> Combine.map (\e -> RecordUpdateExpression fname e)
                        |> Combine.ignore (string "}")

                recordContents : Parser State Expression
                recordContents =
                    Node.parser functionName
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andThen
                            (\fname ->
                                Combine.choice
                                    [ recordUpdateSyntaxParser fname
                                    , string "="
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.continueWith (expression |> Combine.map (\e -> Node.combine Tuple.pair fname e))
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.andThen
                                            (\fieldUpdate ->
                                                Combine.choice
                                                    [ string "}" |> Combine.map (always (RecordExpr [ fieldUpdate ]))
                                                    , string ","
                                                        |> Combine.ignore (maybe Layout.layout)
                                                        |> Combine.continueWith recordFields
                                                        |> Combine.map (\fieldUpdates -> RecordExpr (fieldUpdate :: fieldUpdates))
                                                        |> Combine.ignore (string "}")
                                                    ]
                                            )
                                    ]
                            )
            in
            string "{"
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.continueWith
                    (Combine.choice
                        [ string "}" |> Combine.map (always (RecordExpr []))
                        , recordContents
                        ]
                    )
        )
        |> Node.parser


literalExpression : Parser State (Node Expression)
literalExpression =
    lazy
        (\() ->
            Combine.map Literal (or multiLineStringLiteral stringLiteral)
                |> Node.parser
        )


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Node.parser (Combine.map CharLiteral characterLiteral)



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    lazy
        (\() ->
            succeed (\args expr -> Lambda args expr |> LambdaExpression)
                |> Combine.ignore (string "\\")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andMap (sepBy1 (maybe Layout.layout) functionArgument)
                |> Combine.andMap (Layout.maybeAroundBothSides (string "->") |> Combine.continueWith expression)
                |> Node.parser
        )



-- Case Expression


caseBlock : Parser State (Node Expression)
caseBlock =
    lazy
        (\() ->
            caseToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith expression
                |> Combine.ignore ofToken
        )


caseStatement : Parser State Case
caseStatement =
    lazy
        (\() ->
            succeed Tuple.pair
                |> Combine.andMap pattern
                |> Combine.andMap
                    (maybe (or Layout.layout Layout.layoutStrict)
                        |> Combine.continueWith (string "->")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith expression
                    )
        )


caseStatements : Parser State Cases
caseStatements =
    lazy
        (\() ->
            let
                helper last =
                    Combine.withState
                        (\s ->
                            Combine.withLocation
                                (\l ->
                                    if State.expectedColumn s == l.column then
                                        Combine.choice
                                            [ Combine.map (\c -> Combine.Loop (c :: last)) caseStatement
                                            , Combine.succeed (Combine.Done (List.reverse last))
                                            ]

                                    else
                                        Combine.succeed (Combine.Done (List.reverse last))
                                )
                        )
            in
            caseStatement
                |> Combine.map List.singleton
                |> Combine.andThen (\v -> Combine.loop v helper)
        )


caseExpression : Parser State (Node Expression)
caseExpression =
    lazy
        (\() ->
            Node.parser (Combine.succeed ())
                |> Combine.andThen
                    (\(Node start ()) ->
                        Combine.map
                            (\cb ->
                                Node (Range.combine (start :: List.map (Tuple.second >> Node.range) cb.cases))
                                    (CaseExpression cb)
                            )
                            (succeed CaseBlock
                                |> Combine.andMap caseBlock
                                |> Combine.andMap (Layout.layout |> Combine.continueWith (withIndentedState caseStatements))
                            )
                    )
        )



-- Let Expression


letBody : Parser State (List (Node LetDeclaration))
letBody =
    lazy
        (\() ->
            let
                blockElement : Parser State LetDeclaration
                blockElement =
                    pattern
                        |> Combine.andThen
                            (\(Node r p) ->
                                case p of
                                    VarPattern v ->
                                        functionWithNameNode (Node r v)
                                            |> Combine.map LetFunction

                                    _ ->
                                        letDestructuringDeclarationWithPattern (Node r p)
                            )

                addRange : LetDeclaration -> Node LetDeclaration
                addRange letDeclaration =
                    Node
                        (case letDeclaration of
                            LetFunction letFunction ->
                                Expression.functionRange letFunction

                            LetDestructuring (Node patternRange _) (Node expressionRange _) ->
                                Range.combine [ patternRange, expressionRange ]
                        )
                        letDeclaration
            in
            Combine.succeed (::)
                |> Combine.andMap (blockElement |> Combine.map addRange)
                |> Combine.andMap (many (blockElement |> Combine.map addRange |> Combine.ignore (maybe Layout.layout)))
        )


letDestructuringDeclarationWithPattern : Node Pattern -> Parser State LetDeclaration
letDestructuringDeclarationWithPattern p =
    lazy
        (\() ->
            succeed (LetDestructuring p)
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.ignore (string "=")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andMap expression
        )


letBlock : Parser State (List (Node LetDeclaration))
letBlock =
    lazy
        (\() ->
            (string "let" |> Combine.continueWith Layout.layout)
                |> Combine.continueWith (withIndentedState letBody)
                |> Combine.ignore
                    (choice
                        [ Layout.layout
                        , manySpaces
                        ]
                        |> Combine.continueWith (string "in")
                    )
        )


letExpression : Parser State (Node Expression)
letExpression =
    lazy
        (\() ->
            Ranges.withCurrentPoint
                (\current ->
                    succeed (\decls expr -> Node { start = current.start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression))
                        |> Combine.andMap letBlock
                        |> Combine.andMap (Layout.layout |> Combine.continueWith expression)
                )
        )


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parser (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Ranges.withCurrentPoint
        (\current ->
            ifToken
                |> Combine.continueWith
                    (lazy
                        (\() ->
                            succeed
                                (\condition ifTrue ifFalse ->
                                    Node
                                        { start = current.start, end = (Node.range ifFalse).end }
                                        (IfBlock condition ifTrue ifFalse)
                                )
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andMap expression
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.ignore thenToken
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andMap expression
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andMap
                                    (elseToken
                                        |> Combine.continueWith Layout.layout
                                        |> Combine.continueWith expression
                                    )
                        )
                    )
        )


operatorExpression : Parser State (Node Expression)
operatorExpression =
    let
        negationExpression : Parser State Expression
        negationExpression =
            lazy
                (\() ->
                    Combine.map Negation
                        (choice
                            [ referenceExpression
                            , numberExpression
                            , tupledExpression
                            ]
                            |> Combine.andThen liftRecordAccess
                        )
                )
    in
    lazy
        (\() ->
            Combine.choice
                [ string "-"
                    |> Combine.continueWith (Combine.choice [ negationExpression, succeed (Operator "-") |> Combine.ignore Layout.layout ])
                    |> Node.parser
                , Combine.map Operator infixOperatorToken
                    |> Node.parser
                ]
        )


reference : Parser State ( ModuleName, String )
reference =
    let
        helper : ( String, List String ) -> Parser State ( String, List String )
        helper ( n, xs ) =
            Combine.choice
                [ string "."
                    |> Combine.continueWith
                        (Combine.choice
                            [ Tokens.typeName |> Combine.andThen (\t -> helper ( t, n :: xs ))
                            , Tokens.functionName |> Combine.map (\t -> ( t, n :: xs ))
                            ]
                        )
                , Combine.succeed ( n, xs )
                ]

        recurring : Parser State ( ModuleName, String )
        recurring =
            Tokens.typeName
                |> Combine.andThen (\t -> helper ( t, [] ))
                |> Combine.map (\( t, xs ) -> ( List.reverse xs, t ))

        justFunction : Parser State ( ModuleName, String )
        justFunction =
            Tokens.functionName |> Combine.map (\v -> ( [], v ))
    in
    Combine.choice
        [ recurring
        , justFunction
        ]


referenceExpression : Parser State (Node Expression)
referenceExpression =
    Node.parser
        (reference
            |> Combine.map
                (\( xs, x ) ->
                    FunctionOrValue xs x
                )
        )


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Combine.map ((++) "." >> RecordAccessFunction)
        (string "."
            |> Combine.continueWith functionName
        )
        |> Node.parser


tupledExpression : Parser State (Node Expression)
tupledExpression =
    lazy
        (\() ->
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
                        |> Combine.andMap expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap commaSep

                closingParen =
                    Combine.fromCore (Core.symbol ")")
            in
            Node.parser
                (Combine.fromCore (Core.symbol "(")
                    |> Combine.continueWith
                        (Combine.choice
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
        )
