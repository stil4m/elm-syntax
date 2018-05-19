module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionDeclaration, letBlock, letBody, letExpression, signature)

import Combine exposing (Parser, between, choice, count, fail, lazy, lookAhead, many, many1, maybe, modifyState, or, parens, sepBy, sepBy1, string, succeed, withLocation)
import Combine.Char exposing (anyChar)
import Combine.Num
import Elm.Parser.Base exposing (variablePointer)
import Elm.Parser.Infix as Infix
import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns exposing (declarablePattern, pattern)
import Elm.Parser.Ranges exposing (ranged, withRange, withRangeCustomStart)
import Elm.Parser.State exposing (State, popIndent, pushIndent)
import Elm.Parser.Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDeclaration)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)


declaration : Parser State Declaration
declaration =
    lazy
        (\() ->
            choice
                [ Combine.map AliasDecl Typings.typeAlias
                , Combine.map FuncDecl function
                , Combine.map TypeDecl typeDeclaration
                , portDeclaration
                , infixDeclaration
                , destructuringDeclaration
                ]
        )


function : Parser State Function
function =
    lazy
        (\() ->
            succeed Function
                |> Combine.andMap (succeed Nothing)
                |> Combine.andMap (maybe (ranged signature |> Combine.ignore Layout.layoutStrict))
                |> Combine.andMap functionDeclaration
        )


infixDeclaration : Parser State Declaration
infixDeclaration =
    lazy (\() -> Combine.map InfixDeclaration Infix.infixDefinition)


destructuringDeclaration : Parser State Declaration
destructuringDeclaration =
    lazy
        (\() ->
            succeed Destructuring
                |> Combine.andMap declarablePattern
                |> Combine.andMap (Layout.layout |> Combine.continueWith (string "=") |> Combine.continueWith Layout.layout |> Combine.continueWith expression)
        )


portDeclaration : Parser State Declaration
portDeclaration =
    portToken |> Combine.continueWith (lazy (\() -> Combine.map PortDeclaration (Layout.layout |> Combine.continueWith signature)))


signature : Parser State FunctionSignature
signature =
    succeed FunctionSignature
        |> Combine.andMap (lookAhead anyChar |> Combine.andThen (\c -> succeed (c == '(')))
        |> Combine.andMap (or functionName (parens prefixOperatorToken))
        |> Combine.andMap (Layout.maybeAroundBothSides (string ":") |> Combine.continueWith (maybe Layout.layout) |> Combine.continueWith typeAnnotation)


functionDeclaration : Parser State FunctionDeclaration
functionDeclaration =
    lazy
        (\() ->
            succeed FunctionDeclaration
                |> Combine.andMap (lookAhead anyChar |> Combine.andThen (\c -> succeed (c == '(')))
                |> Combine.andMap (variablePointer <| or functionName (parens prefixOperatorToken))
                |> Combine.andMap (many (Layout.layout |> Combine.continueWith functionArgument))
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string "=")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith expression
                    )
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
                    [ unitExpression
                    , qualifiedExpression
                    , functionOrValueExpression
                    , ifBlockExpression
                    , prefixOperatorExpression
                    , tupledExpression
                    , recordAccessFunctionExpression
                    , negationExpression
                    , operatorExpression
                    , floatableExpression
                    , integerExpression
                    , letExpression
                    , lambdaExpression
                    , literalExpression
                    , charLiteralExpression
                    , recordExpression
                    , recordUpdateExpression
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
            maybe Layout.layout
                |> Combine.continueWith expressionNotApplication
                |> Combine.andThen
                    (\expr ->
                        or (promoteToApplicationExpression expr)
                            (succeed expr)
                    )
        )


promoteToApplicationExpression : Ranged Expression -> Parser State (Ranged Expression)
promoteToApplicationExpression expr =
    lazy
        (\() ->
            succeed (\rest -> Application (expr :: rest))
                |> Combine.andMap (lazy (\() -> many1 (maybe Layout.layout |> Combine.continueWith expressionNotApplication)))
                |> rangedExpressionWithStart (Tuple.first expr)
        )



-- End expression


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            (modifyState (pushIndent location.column) |> Combine.continueWith p)
                |> Combine.ignore (modifyState popIndent)
        )


unitExpression : Parser State Expression
unitExpression =
    Combine.map (always UnitExpr) (string "()")


glslExpression : Parser State Expression
glslExpression =
    between (string "[glsl|")
        (string "|]")
        (many
            (lookAhead (Combine.map String.fromList (count 2 anyChar))
                |> Combine.andThen
                    (\s ->
                        if s == "|]" then
                            fail "end symbol"

                        else
                            anyChar
                    )
            )
        )
        |> Combine.map (String.fromList >> GLSLExpression)



-- listExpression


listExpression : Parser State Expression
listExpression =
    lazy
        (\() ->
            or emptyListExpression
                (Combine.map ListExpr
                    (between
                        (string "[")
                        (string "]")
                        (sepBy (string ",")
                            (Layout.maybeAroundBothSides expression)
                        )
                    )
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


recordExpressionField : Parser State ( String, Ranged Expression )
recordExpressionField =
    lazy
        (\() ->
            succeed (\a b -> ( a, b ))
                |> Combine.andMap functionName
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string "=")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith expression
                    )
        )


recordFields : Bool -> Parser State (List ( String, Ranged Expression ))
recordFields oneOrMore =
    let
        p =
            if oneOrMore then
                sepBy1

            else
                sepBy
    in
    p (string ",") (Layout.maybeAroundBothSides recordExpressionField)


recordExpression : Parser State Expression
recordExpression =
    lazy
        (\() ->
            Combine.map RecordExpr
                (between (string "{")
                    (string "}")
                    (recordFields False)
                )
        )


recordUpdateExpression : Parser State Expression
recordUpdateExpression =
    lazy
        (\() ->
            between (string "{")
                (string "}")
                (Combine.map RecordUpdateExpression
                    (succeed RecordUpdate
                        |> Combine.andMap (Layout.maybeAroundBothSides functionName)
                        |> Combine.andMap (string "|" |> Combine.continueWith (recordFields True))
                    )
                )
        )



-- literalExpression


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
                |> Combine.andMap
                    (string "\\"
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith (sepBy1 Layout.layout functionArgument)
                    )
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
                |> Combine.ignore Layout.layout
                |> Combine.ignore ofToken
        )


caseStatement : Parser State Case
caseStatement =
    lazy
        (\() ->
            succeed (\a b -> ( a, b ))
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
    lazy (\() -> sepBy1 Layout.layoutStrict caseStatement)


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
            sepBy1 Layout.layoutStrict (ranged (or letDestructuringDeclaration (Combine.map LetFunction function)))
        )


letDestructuringDeclaration : Parser State LetDeclaration
letDestructuringDeclaration =
    lazy
        (\() ->
            succeed LetDestructuring
                |> Combine.andMap declarablePattern
                |> Combine.andMap
                    (Layout.layout
                        |> Combine.continueWith (string "=")
                        |> Combine.continueWith Layout.layout
                        |> Combine.continueWith expression
                    )
        )


letBlock : Parser State (List (Ranged LetDeclaration))
letBlock =
    lazy
        (\() ->
            (string "let" |> Combine.continueWith Layout.layout)
                |> Combine.continueWith (withIndentedState letBody)
                |> Combine.ignore (Combine.map String.fromList (lookAhead (many anyChar)))
                |> Combine.ignore
                    (choice
                        [ Layout.layout
                        , Combine.map (always ()) manySpaces
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


integerExpression : Parser State Expression
integerExpression =
    Combine.map Integer Combine.Num.int


floatableExpression : Parser State Expression
floatableExpression =
    Combine.map Floatable Combine.Num.float


ifBlockExpression : Parser State Expression
ifBlockExpression =
    ifToken
        |> Combine.continueWith
            (lazy
                (\() ->
                    succeed IfBlock
                        |> Combine.andMap (Layout.maybeAroundBothSides expression)
                        |> Combine.andMap (thenToken |> Combine.continueWith (Layout.maybeAroundBothSides expression))
                        |> Combine.andMap (elseToken |> Combine.continueWith Layout.layout |> Combine.continueWith expression)
                )
            )


prefixOperatorExpression : Parser State Expression
prefixOperatorExpression =
    Combine.map PrefixOperator (parens prefixOperatorToken)


negationExpression : Parser State Expression
negationExpression =
    lazy
        (\() ->
            Combine.map Negation
                (string "-"
                    |> Combine.continueWith
                        (rangedExpression
                            (choice
                                [ qualifiedExpression
                                , functionOrValueExpression
                                , integerExpression
                                , floatableExpression
                                , tupledExpression
                                ]
                            )
                            |> Combine.andThen liftRecordAccess
                        )
                )
        )


operatorExpression : Parser State Expression
operatorExpression =
    Combine.map Operator infixOperatorToken


functionOrValueExpression : Parser State Expression
functionOrValueExpression =
    lazy
        (\() ->
            Combine.map FunctionOrValue (choice [ functionName, typeName ])
        )


qualifiedExpression : Parser State Expression
qualifiedExpression =
    lazy
        (\() ->
            succeed QualifiedExpr
                |> Combine.andMap (many1 (typeName |> Combine.ignore (string ".")))
                |> Combine.andMap (or functionName typeName)
        )


recordAccessFunctionExpression : Parser State Expression
recordAccessFunctionExpression =
    Combine.map ((++) "." >> RecordAccessFunction) (string "." |> Combine.continueWith functionName)


tupledExpression : Parser State Expression
tupledExpression =
    lazy
        (\() ->
            parens (sepBy1 (string ",") (Layout.maybeAroundBothSides expression))
                |> Combine.map
                    (\l ->
                        case l of
                            [ x ] ->
                                ParenthesizedExpression x

                            xs ->
                                TupledExpression xs
                    )
        )
