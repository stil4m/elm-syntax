module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionArgument, functionSignature, letBlock, letBody, letExpression, signature)

import Combine exposing (($>), (<$>), (>>=), Parser, between, choice, count, fail, lazy, lookAhead, many, maybe, modifyState, or, sepBy1, string, succeed, withLocation)
import Combine.Char exposing (anyChar)
import Combine.Extra as Combine
import Elm.Parser.Base exposing (variablePointer)
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.Ranges as Ranges exposing (ranged, withRange)
import Elm.Parser.State as State exposing (State, popIndent, pushIndent)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionDeclaration, FunctionSignature, Lambda, LetBlock, LetDeclaration(..), RecordUpdate)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range as Range
import Elm.Syntax.Ranged exposing (Ranged)


declaration : Parser State (Ranged Declaration)
declaration =
    lazy
        (\() ->
            choice
                [ typeDefinition
                    |> Combine.map
                        (\v ->
                            case v of
                                Typings.DefinedType r t ->
                                    ( r, TypeDecl t )

                                Typings.DefinedAlias r a ->
                                    ( r, AliasDecl a )
                        )
                , function
                , portDeclaration
                , infixDeclaration
                , destructuringDeclaration
                ]
        )


functionSignatureFromVarPointer : ( Bool, VariablePointer ) -> Parser State (Ranged FunctionSignature)
functionSignatureFromVarPointer ( op, var ) =
    succeed (\ta -> ( Range.combine [ var.range, Tuple.first ta ], FunctionSignature op var ta ))
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andMap typeAnnotation


functionSignature : Parser State (Ranged FunctionSignature)
functionSignature =
    succeed (\b var -> ( b, var ))
        |> Combine.andMap (lookAhead anyChar >>= (\c -> succeed (c == '(')))
        |> Combine.andMap (variablePointer (or functionName (Combine.parens prefixOperatorToken)))
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionSignatureFromVarPointer


functionWithVariablePointer : ( Bool, VariablePointer ) -> Parser State Function
functionWithVariablePointer pointer =
    let
        functionDeclFromVarPointer : ( Bool, VariablePointer ) -> Parser State FunctionDeclaration
        functionDeclFromVarPointer ( op, var ) =
            succeed (FunctionDeclaration op var)
                |> Combine.andMap (many (functionArgument |> Combine.ignore (maybe Layout.layout)))
                |> Combine.ignore (string "=")
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andMap expression

        fromParts : Ranged FunctionSignature -> FunctionDeclaration -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : ( Bool, VariablePointer ) -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.andThen
                    (\sig ->
                        maybe Layout.layoutStrict
                            |> Combine.continueWith (variablePointer (or functionName (Combine.parens prefixOperatorToken)))
                            |> Combine.map (\v -> ( Tuple.first varPointer, v ))
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andThen functionDeclFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : ( Bool, VariablePointer ) -> Parser State Function
        functionWithoutSignature varPointer =
            functionDeclFromVarPointer varPointer
                |> Combine.map (Function Nothing Nothing)
    in
    Combine.choice
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


function : Parser State (Ranged Declaration)
function =
    lazy
        (\() ->
            succeed (,)
                |> Combine.andMap (lookAhead anyChar >>= (\c -> succeed (c == '(')))
                |> Combine.andMap (variablePointer (or functionName (Combine.parens prefixOperatorToken)))
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andThen functionWithVariablePointer
                |> Combine.map (\f -> ( Expression.functionRange f, FuncDecl f ))
        )


signature : Parser State FunctionSignature
signature =
    succeed FunctionSignature
        |> Combine.andMap (lookAhead anyChar >>= (\c -> succeed (c == '(')))
        |> Combine.andMap (variablePointer functionName)
        |> Combine.andMap (Layout.maybeAroundBothSides (string ":") |> Combine.continueWith (maybe Layout.layout) |> Combine.continueWith typeAnnotation)


infixDeclaration : Parser State (Ranged Declaration)
infixDeclaration =
    lazy (\() -> ranged (InfixDeclaration <$> Infix.infixDefinition))


destructuringDeclaration : Parser State (Ranged Declaration)
destructuringDeclaration =
    lazy
        (\() ->
            succeed
                (\x y ->
                    ( Range.combine [ Tuple.first x, Tuple.first y ]
                    , Destructuring x y
                    )
                )
                |> Combine.andMap pattern
                |> Combine.ignore (string "=")
                |> Combine.ignore Layout.layout
                |> Combine.andMap expression
        )


portDeclaration : Parser State (Ranged Declaration)
portDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            portToken
                |> Combine.ignore Layout.layout
                |> Combine.continueWith signature
                |> Combine.map (\sig -> ( Range.combine [ current, Tuple.first sig.typeAnnotation ], PortDeclaration sig ))
        )


functionArgument : Parser State (Ranged Pattern)
functionArgument =
    pattern



-- Expressions


rangedExpression : Parser State Expression -> Parser State (Ranged Expression)
rangedExpression p =
    withRange <| Combine.map (\a b -> ( b, a )) p


expressionNotApplication : Parser State (Ranged Expression)
expressionNotApplication =
    lazy
        (\() ->
            (rangedExpression <|
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
                |> Combine.map fixRange
                |> Combine.andThen liftRecordAccess
        )


fixRange : Ranged Expression -> Ranged Expression
fixRange ( r, e ) =
    case e of
        -- Needed while case blocks consume whitespace eagerly
        CaseExpression caseExpr ->
            ( caseExpr.cases
                |> List.reverse
                |> List.head
                |> Maybe.map (\( _, ( lastCaseExprRange, _ ) ) -> { r | end = lastCaseExprRange.end })
                |> Maybe.withDefault r
            , e
            )

        _ ->
            ( r, e )


liftRecordAccess : Ranged Expression -> Parser State (Ranged Expression)
liftRecordAccess e =
    or ((rangedExpression <| Combine.map (RecordAccess e) (string "." |> Combine.continueWith functionName)) |> Combine.andThen liftRecordAccess)
        (succeed e)


expression : Parser State (Ranged Expression)
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
                                            ( Range.combine (Tuple.first first :: List.map Tuple.first rest)
                                            , Application (first :: List.reverse rest)
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
                        promoter []
                    )
        )



-- End expression


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            (modifyState (pushIndent location.column) |> Combine.continueWith p)
                |> Combine.ignore (modifyState popIndent)
        )


glslExpression : Parser State Expression
glslExpression =
    (String.fromList >> GLSLExpression)
        <$> between (string "[glsl|")
                (string "|]")
                (many
                    (lookAhead (String.fromList <$> count 2 anyChar)
                        >>= (\s ->
                                if s == "|]" then
                                    fail "end symbol"
                                else
                                    anyChar
                            )
                    )
                )


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



-- recordExpression


recordExpression : Parser State Expression
recordExpression =
    lazy
        (\() ->
            let
                recordField : Parser State ( String, Ranged Expression )
                recordField =
                    succeed (,)
                        |> Combine.andMap functionName
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string "=")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap expression

                recordFields : Parser State (List ( String, Ranged Expression ))
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

                recordContents : Parser State Expression
                recordContents =
                    functionName
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andThen
                            (\fname ->
                                Combine.choice
                                    [ string "|"
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.continueWith recordFields
                                        |> Combine.map (RecordUpdate fname >> RecordUpdateExpression)
                                        |> Combine.ignore (string "}")
                                    , string "="
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.continueWith (expression |> Combine.map (\e -> ( fname, e )))
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
    Combine.map Literal (or multiLineStringLiteral stringLiteral)


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


caseBlock : Parser State (Ranged Expression)
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
            succeed (,)
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
                                    if State.currentIndent s == l.column then
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


letBody : Parser State (List (Ranged LetDeclaration))
letBody =
    lazy
        (\() ->
            let
                blockElement =
                    pattern
                        |> Combine.andThen
                            (\( r, p ) ->
                                case p of
                                    VarPattern v ->
                                        functionWithVariablePointer ( False, VariablePointer v r )
                                            |> Combine.map LetFunction

                                    _ ->
                                        letDestructuringDeclarationWithPattern ( r, p )
                            )
            in
            Combine.succeed (::)
                |> Combine.andMap (ranged blockElement)
                |> Combine.andMap (many (ranged blockElement |> Combine.ignore (maybe Layout.layout)))
        )


letDestructuringDeclarationWithPattern : Ranged Pattern -> Parser State LetDeclaration
letDestructuringDeclarationWithPattern p =
    succeed (LetDestructuring p)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.andMap expression


letBlock : Parser State (List (Ranged LetDeclaration))
letBlock =
    lazy
        (\() ->
            (string "let" |> Combine.continueWith Layout.layout)
                |> Combine.continueWith (withIndentedState letBody)
                |> Combine.ignore
                    (choice
                        [ Layout.layout
                        , manySpaces $> ()
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
    Elm.Parser.Numbers.number Floatable Integer


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
                        (rangedExpression
                            (choice
                                [ referenceExpression
                                , numberExpression
                                , tupledExpression
                                ]
                            )
                            |> Combine.andThen liftRecordAccess
                        )
                 -- )
                )
    in
    Combine.choice
        [ string "-"
            |> Combine.continueWith (Combine.choice [ negationExpression, succeed (Operator "-") |> Combine.ignore Layout.layout ])
        , Combine.map Operator infixOperatorToken
        ]


reference : Parser s ( ModuleName, String )
reference =
    let
        helper ( n, xs ) =
            Combine.choice
                [ string "."
                    |> Combine.continueWith (Combine.choice [ Tokens.typeName, Tokens.functionName ])
                    |> Combine.andThen (\t -> helper ( t, n :: xs ))
                , Combine.succeed ( n, xs )
                ]
    in
    Combine.choice
        [ Tokens.typeName
            |> Combine.andThen (\t -> helper ( t, [] ))
            |> Combine.map (\( t, xs ) -> ( List.reverse xs, t ))
        , Tokens.functionName |> Combine.map (\v -> ( [], v ))
        ]


referenceExpression : Parser State Expression
referenceExpression =
    reference
        |> Combine.map
            (\( xs, x ) ->
                case xs of
                    [] ->
                        FunctionOrValue x

                    _ ->
                        QualifiedExpr xs x
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
        (\() ->
            let
                asExpression : Ranged Expression -> List (Ranged Expression) -> Expression
                asExpression x xs =
                    case xs of
                        [] ->
                            ParenthesizedExpression x

                        _ ->
                            TupledExpression (x :: xs)

                commaSep : Parser State (List (Ranged Expression))
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
                    Combine.string ")"
            in
            Combine.string "("
                |> Combine.continueWith
                    (Combine.choice
                        [ closingParen |> Combine.map (always UnitExpr)
                        , -- Backtracking needed for record access expression
                          -- Combine.backtrackable
                          prefixOperatorToken
                            |> Combine.ignore closingParen
                            |> Combine.map PrefixOperator
                        , nested |> Combine.ignore closingParen
                        ]
                    )
        )
