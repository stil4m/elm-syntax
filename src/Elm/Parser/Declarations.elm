module Elm.Parser.Declarations exposing (declaration, expression, letExpression)

import Combine exposing (Parser, lazy, many, maybe, modifyState, oneOf, or, sepBy1, string, succeed, withLocation)
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.Ranges as Ranges
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
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
            succeed (\args expr -> Node (Range.combine [ Node.range varPointer, Node.range expr ]) (FunctionImplementation varPointer args expr))
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
    Ranges.withCurrentPoint
        (\current ->
            Infix.infixDefinition
                |> Combine.map (\inf -> Node (Range.combine [ current, Node.range inf.function ]) (Declaration.InfixDeclaration inf))
        )


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            portToken
                |> Combine.ignore Layout.layout
                |> Combine.continueWith signature
                |> Combine.map (\sig -> Node (Range.combine [ current, Node.range sig.typeAnnotation ]) (Declaration.PortDeclaration sig))
        )


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
            |> Combine.map (\f -> Node.combine RecordAccess e f)
            |> Combine.andThen liftRecordAccess
        )
        (succeed e)


expression : Parser State (Node Expression)
expression =
    expressionNotApplication
        |> Combine.andThen
            (\first ->
                let
                    complete : List (Node Expression) -> Parser s (Node Expression)
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

                    promoter : List (Node Expression) -> Parser State (Node Expression)
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
                |> Combine.keep (many (string "," |> Combine.ignore (maybe Layout.layout) |> Combine.continueWith expression))
                |> Combine.map ListExpr
    in
    string "["
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ string "]" |> Combine.map (always (ListExpr []))
                , innerExpressions |> Combine.ignore (string "]")
                ]
            )
        |> Node.parser



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    (let
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
                        Combine.oneOf
                            [ recordUpdateSyntaxParser fname
                            , string "="
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.continueWith (expression |> Combine.map (\e -> Node.combine Tuple.pair fname e))
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andThen
                                    (\fieldUpdate ->
                                        Combine.oneOf
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
            (Combine.oneOf
                [ string "}" |> Combine.map (always (RecordExpr []))
                , recordContents
                ]
            )
    )
        |> Node.parser


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
    Ranges.withCurrentPoint
        (\current ->
            succeed
                (\args expr ->
                    Lambda args expr
                        |> LambdaExpression
                        |> Node { start = current.start, end = (Node.range expr).end }
                )
                |> Combine.ignore (string "\\")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep (sepBy1 (maybe Layout.layout) functionArgument)
                |> Combine.keep (Layout.maybeAroundBothSides (string "->") |> Combine.continueWith expression)
        )



-- Case Expression


caseBlock : Parser State (Node Expression)
caseBlock =
    caseToken
        |> Combine.continueWith Layout.layout
        |> Combine.continueWith expression
        |> Combine.ignore ofToken


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
    let
        helper : ( Range, List Case ) -> Parser State (Combine.Step ( Range, List Case ) ( Range, List Case ))
        helper ( endRange, last ) =
            Combine.withState
                (\s ->
                    Combine.withLocation
                        (\l ->
                            if State.expectedColumn s == l.column then
                                Combine.oneOf
                                    [ Combine.map (\c -> Combine.Loop ( Node.range (Tuple.second c), c :: last )) caseStatement
                                    , Combine.succeed (Combine.Done ( endRange, last ))
                                    ]

                            else
                                Combine.succeed (Combine.Done ( endRange, last ))
                        )
                )
    in
    caseStatement
        |> Combine.andThen (\v -> Combine.loop ( Node.range (Tuple.second v), [ v ] ) helper)
        |> Combine.map (\( endRange, cases ) -> ( endRange.end, List.reverse cases ))


caseExpression : Parser State (Node Expression)
caseExpression =
    Combine.withLocation
        (\start ->
            succeed
                (\caseBlock_ ( end, cases ) ->
                    Node { start = start, end = end }
                        (CaseExpression (CaseBlock caseBlock_ cases))
                )
                |> Combine.keep caseBlock
                |> Combine.ignore Layout.layout
                |> Combine.keep (withIndentedState caseStatements)
        )



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


letBlock : Parser State (List (Node LetDeclaration))
letBlock =
    (string "let" |> Combine.continueWith Layout.layout)
        |> Combine.continueWith (withIndentedState letBody)
        |> Combine.ignore
            (oneOf
                [ Layout.layout
                , manySpaces
                ]
                |> Combine.continueWith (string "in")
            )


letExpression : Parser State (Node Expression)
letExpression =
    Ranges.withCurrentPoint
        (\current ->
            succeed (\decls expr -> Node { start = current.start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression))
                |> Combine.keep letBlock
                |> Combine.keep (Layout.layout |> Combine.continueWith expression)
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
                    (succeed
                        (\condition ifTrue ifFalse ->
                            Node
                                { start = current.start, end = (Node.range ifFalse).end }
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
