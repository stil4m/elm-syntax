module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionArgument, functionSignature, letBlock, letBody, letExpression, signature)

import Combine exposing (Parser, between, choice, count, fail, lazy, many, many1, maybe, modifyState, or, sepBy, sepBy1, string, succeed, withLocation)
import Combine.Num
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.Ranges as Ranges exposing (withRange)
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces, realNewLine)
import Elm.Syntax.Declaration as Declaration exposing (..)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range as Range exposing (Range, emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing (Nestable(..))


declaration : Parser State (Node Declaration)
declaration =
    lazy
        (\() ->
            choice
                [ function
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
                , infixDeclaration
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
            (Node.parser <|
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
            )
                |> Combine.andThen liftRecordAccess
        )


liftRecordAccess : Node Expression -> Parser State (Node Expression)
liftRecordAccess e =
    lazy
        (\() ->
            or
                ((Node.parser <|
                    Combine.map (RecordAccess e)
                        (string "." |> Combine.continueWith (Node.parser functionName))
                 )
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
                                succeed <|
                                    case rest of
                                        [] ->
                                            first

                                        _ ->
                                            Node
                                                (Range.combine (Node.range first :: List.map Node.range rest))
                                                (Application (first :: List.reverse rest))

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


glslExpression : Parser State Expression
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


listExpression : Parser State Expression
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
        )


emptyListExpression : Parser State Expression
emptyListExpression =
    lazy
        (\() ->
            Combine.map (always (ListExpr []))
                (string "["
                    |> Combine.continueWith (maybe (or Layout.layout Layout.layoutAndNewLine))
                    |> Combine.continueWith (string "]")
                )
        )



-- recordExpression


recordExpression : Parser State Expression
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


literalExpression : Parser State Expression
literalExpression =
    lazy
        (\() ->
            Combine.map Literal (or multiLineStringLiteral stringLiteral)
        )


charLiteralExpression : Parser State Expression
charLiteralExpression =
    Combine.map CharLiteral characterLiteral



-- lambda


lambdaExpression : Parser State Expression
lambdaExpression =
    lazy
        (\() ->
            succeed (\args expr -> Lambda args expr |> LambdaExpression)
                |> Combine.ignore (string "\\")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andMap (sepBy1 (maybe Layout.layout) functionArgument)
                |> Combine.andMap (Layout.maybeAroundBothSides (string "->") |> Combine.continueWith expression)
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
                                        caseStatement
                                            |> Combine.map (\c -> c :: last)
                                            |> Combine.andThen helper

                                    else
                                        succeed last
                                )
                        )
            in
            caseStatement
                |> Combine.map List.singleton
                |> Combine.andThen helper
                |> Combine.map List.reverse
        )


caseExpression : Parser State Expression
caseExpression =
    lazy
        (\() ->
            Combine.map CaseExpression
                (succeed CaseBlock
                    |> Combine.andMap caseBlock
                    |> Combine.andMap (Layout.layout |> Combine.continueWith (withIndentedState caseStatements))
                )
        )



-- Let Expression


letBody : Parser State (List (Node LetDeclaration))
letBody =
    lazy
        (\() ->
            let
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
            in
            Combine.succeed (::)
                |> Combine.andMap (Node.parser blockElement)
                |> Combine.andMap (many (Node.parser blockElement |> Combine.ignore (maybe Layout.layout)))
        )


letDestructuringDeclarationWithPattern : Node Pattern -> Parser State LetDeclaration
letDestructuringDeclarationWithPattern p =
    lazy
        (\() ->
            succeed (LetDestructuring p)
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.ignore (string "=")
                |> Combine.ignore Layout.layout
                |> Combine.andMap expression
        )


letDestructuringDeclaration : Parser State LetDeclaration
letDestructuringDeclaration =
    lazy (\() -> Combine.andThen letDestructuringDeclarationWithPattern pattern)


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


letExpression : Parser State Expression
letExpression =
    lazy
        (\() ->
            succeed (\decls -> LetBlock decls >> LetExpression)
                |> Combine.andMap letBlock
                |> Combine.andMap (Layout.layout |> Combine.continueWith expression)
        )


numberExpression : Parser State Expression
numberExpression =
    Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex


ifBlockExpression : Parser State Expression
ifBlockExpression =
    ifToken
        |> Combine.continueWith
            (lazy
                (\() ->
                    succeed IfBlock
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore thenToken
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap expression
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap (elseToken |> Combine.continueWith Layout.layout |> Combine.continueWith expression)
                )
            )


operatorExpression : Parser State Expression
operatorExpression =
    let
        negationExpression : Parser State Expression
        negationExpression =
            lazy
                (\() ->
                    Combine.map Negation
                        (Node.parser
                            (choice
                                [ referenceExpression
                                , numberExpression
                                , tupledExpression
                                ]
                            )
                            |> Combine.andThen liftRecordAccess
                        )
                )
    in
    lazy
        (\() ->
            Combine.choice
                [ string "-"
                    |> Combine.continueWith (Combine.choice [ negationExpression, succeed (Operator "-") |> Combine.ignore Layout.layout ])
                , Combine.map Operator infixOperatorToken
                ]
        )


reference : Parser State ( ModuleName, String )
reference =
    let
        helper : ( String, List String ) -> Parser State ( String, List String )
        helper ( n, xs ) =
            Combine.choice
                [ string "."
                    |> Combine.continueWith (Combine.choice [ Tokens.typeName, Tokens.functionName ])
                    |> Combine.andThen (\t -> helper ( t, n :: xs ))
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


referenceExpression : Parser State Expression
referenceExpression =
    reference
        |> Combine.map
            (\( xs, x ) ->
                FunctionOrValue xs x
            )


recordAccessFunctionExpression : Parser State Expression
recordAccessFunctionExpression =
    Combine.map ((++) "." >> RecordAccessFunction)
        (string "."
            |> Combine.continueWith functionName
        )


tupledExpression : Parser State Expression
tupledExpression =
    lazy
        (\v ->
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
            Combine.fromCore (Core.symbol "(")
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
