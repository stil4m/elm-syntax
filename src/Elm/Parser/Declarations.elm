module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionArgument, functionSignature, letBlock, letBody, letExpression, signature)

import Combine exposing (Parser, between, choice, count, fail, lazy, many, many1, maybe, modifyState, or, sepBy, sepBy1, string, succeed, withLocation)
import Combine.Char exposing (anyChar)
import Combine.Num
import Elm.Parser.Base exposing (variablePointer)
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.Ranges as Ranges exposing (ranged, withRange, withRangeCustomStart)
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDefinition)
import Elm.Parser.Whitespace exposing (manySpaces, realNewLine)
import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Declaration as Declaration exposing (..)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionDeclaration, FunctionSignature, Lambda, LetBlock, LetDeclaration(..), RecordUpdate)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range as Range exposing (Range, emptyRange)
import Elm.Syntax.Ranged exposing (Ranged)
import Parser as Core exposing (Nestable(..))


declaration : Parser State (Ranged Declaration)
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
                                    ( r, TypeDecl t )

                                Typings.DefinedAlias r a ->
                                    ( r, AliasDecl a )
                        )
                , portDeclaration
                , infixDeclaration
                , destructuringDeclaration
                ]
        )


functionSignatureFromVarPointer : VariablePointer -> Parser State (Ranged FunctionSignature)
functionSignatureFromVarPointer varPointer =
    succeed (\ta -> ( Range.combine [ varPointer.range, Tuple.first ta ], FunctionSignature varPointer ta ))
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andMap typeAnnotation


functionSignature : Parser State (Ranged FunctionSignature)
functionSignature =
    variablePointer functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen functionSignatureFromVarPointer


functionWithVariablePointer : VariablePointer -> Parser State Function
functionWithVariablePointer pointer =
    let
        functionDeclFromVarPointer : VariablePointer -> Parser State FunctionDeclaration
        functionDeclFromVarPointer varPointer =
            succeed (FunctionDeclaration varPointer)
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

        functionWithSignature : VariablePointer -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.andThen
                    (\sig ->
                        maybe Layout.layoutStrict
                            |> Combine.continueWith (variablePointer functionName)
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.andThen functionDeclFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : VariablePointer -> Parser State Function
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
            variablePointer functionName
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.andThen functionWithVariablePointer
                |> Combine.map (\f -> ( Expression.functionRange f, FuncDecl f ))
        )


signature : Parser State FunctionSignature
signature =
    succeed FunctionSignature
        |> Combine.andMap (variablePointer functionName)
        |> Combine.andMap (Layout.maybeAroundBothSides (string ":") |> Combine.continueWith (maybe Layout.layout) |> Combine.continueWith typeAnnotation)


infixDeclaration : Parser State (Ranged Declaration)
infixDeclaration =
    Ranges.withCurrentPoint
        (\current ->
            Infix.infixDefinition
                |> Combine.map (\inf -> ( Range.combine [ current, Tuple.first inf.function ], InfixDeclaration inf ))
        )


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


rangedExpressionWithStart : Range -> Parser State Expression -> Parser State (Ranged Expression)
rangedExpressionWithStart r p =
    withRangeCustomStart r <| Combine.map (\a b -> ( b, a )) p


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
                |> Combine.andThen liftRecordAccess
        )


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
    Combine.map (always (ListExpr []))
        (string "["
            |> Combine.continueWith (maybe (or Layout.layout Layout.layoutAndNewLine))
            |> Combine.continueWith (string "]")
        )



-- recordExpression


recordExpression : Parser State Expression
recordExpression =
    lazy
        (\() ->
            let
                recordField : Parser State ( String, Ranged Expression )
                recordField =
                    succeed Tuple.pair
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
                                        functionWithVariablePointer (VariablePointer v r)
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


letDestructuringDeclaration : Parser State LetDeclaration
letDestructuringDeclaration =
    lazy (\() -> Combine.andThen letDestructuringDeclarationWithPattern pattern)


letBlock : Parser State (List (Ranged LetDeclaration))
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
        (\v ->
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
