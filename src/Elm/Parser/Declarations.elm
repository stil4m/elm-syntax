module Elm.Parser.Declarations exposing (caseBlock, caseStatement, caseStatements, declaration, expression, function, functionDeclaration, letBlock, letBody, signature)

import Combine exposing ((*>), (<$), (<$>), (<*), (<*>), (>>=), Parser, between, choice, count, fail, lazy, lookAhead, many, many1, maybe, modifyState, or, parens, sepBy, sepBy1, string, succeed, withLocation)
import Combine.Char exposing (anyChar)
import Combine.Num
import Elm.Parser.Infix as Infix
import Elm.Parser.Patterns exposing (declarablePattern, pattern)
import Elm.Parser.Ranges exposing (withRange, withRangeCustomStart)
import Elm.Parser.State exposing (State, popIndent, pushIndent)
import Elm.Parser.Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, infixOperatorToken, multiLineStringLiteral, ofToken, portToken, prefixOperatorToken, stringLiteral, thenToken, typeName)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Typings as Typings exposing (typeDeclaration)
import Elm.Parser.Util exposing (asPointer, commentSequence, exactIndentWhitespace, moreThanIndentWhitespace, trimmed, unstrictIndentWhitespace)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Range)
import List.Extra as List


declaration : Parser State Declaration
declaration =
    lazy
        (\() ->
            choice
                [ AliasDecl <$> Typings.typeAlias
                , FuncDecl <$> function
                , TypeDecl <$> typeDeclaration
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
                <*> succeed Nothing
                <*> maybe (signature <* exactIndentWhitespace)
                <*> functionDeclaration
        )


infixDeclaration : Parser State Declaration
infixDeclaration =
    lazy (\() -> InfixDeclaration <$> Infix.infixDefinition)


destructuringDeclaration : Parser State Declaration
destructuringDeclaration =
    lazy
        (\() ->
            succeed Destructuring
                <*> declarablePattern
                <*> (moreThanIndentWhitespace *> string "=" *> moreThanIndentWhitespace *> expression)
        )


portDeclaration : Parser State Declaration
portDeclaration =
    portToken *> lazy (\() -> PortDeclaration <$> (moreThanIndentWhitespace *> signature))


signature : Parser State FunctionSignature
signature =
    withRange <|
        succeed FunctionSignature
            <*> (lookAhead anyChar >>= (\c -> succeed (c == '(')))
            <*> or functionName (parens prefixOperatorToken)
            <*> (trimmed (string ":") *> maybe moreThanIndentWhitespace *> typeAnnotation)


functionDeclaration : Parser State FunctionDeclaration
functionDeclaration =
    lazy
        (\() ->
            succeed FunctionDeclaration
                <*> (lookAhead anyChar >>= (\c -> succeed (c == '(')))
                <*> (asPointer <| or functionName (parens prefixOperatorToken))
                <*> many (moreThanIndentWhitespace *> functionArgument)
                <*> (maybe moreThanIndentWhitespace
                        *> string "="
                        *> maybe moreThanIndentWhitespace
                        *> expression
                    )
        )


functionArgument : Parser State Pattern
functionArgument =
    pattern



-- Expressions


rangedExpression : Parser State InnerExpression -> Parser State Expression
rangedExpression p =
    withRange <| (flip (,) <$> p)


rangedExpressionWithStart : Range -> Parser State InnerExpression -> Parser State Expression
rangedExpressionWithStart r p =
    withRangeCustomStart r <| (flip (,) <$> p)


expressionNotApplication : Parser State Expression
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
                >>= liftRecordAccess
        )


liftRecordAccess : Expression -> Parser State Expression
liftRecordAccess e =
    or ((rangedExpression <| RecordAccess e <$> (string "." *> functionName)) >>= liftRecordAccess)
        (succeed e)


expression : Parser State Expression
expression =
    lazy
        (\() ->
            expressionNotApplication
                >>= (\expr ->
                        or (promoteToApplicationExpression expr)
                            (succeed expr)
                    )
        )


promoteToApplicationExpression : Expression -> Parser State Expression
promoteToApplicationExpression expr =
    lazy
        (\() ->
            rangedExpressionWithStart (Tuple.first expr) <|
                succeed (\rest -> Application (expr :: rest))
                    <*> lazy (\() -> many1 (maybe moreThanIndentWhitespace *> expressionNotApplication))
        )



-- End expression


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            (modifyState (pushIndent (location.column + 1)) *> p)
                <* modifyState popIndent
        )


withIndentedState2 : Parser State a -> Parser State a
withIndentedState2 p =
    withLocation
        (\location ->
            let
                x =
                    location.source
                        |> String.toList
                        |> List.takeWhile ((==) ' ')
                        |> List.length
            in
            (modifyState (pushIndent x) *> p)
                <* modifyState popIndent
        )


unitExpression : Parser State InnerExpression
unitExpression =
    UnitExpr <$ string "()"


glslExpression : Parser State InnerExpression
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



-- listExpression


listExpression : Parser State InnerExpression
listExpression =
    lazy
        (\() ->
            or emptyListExpression
                (ListExpr
                    <$> between
                            (string "[")
                            (string "]")
                            (sepBy (string ",")
                                (trimmed expression)
                            )
                )
        )


emptyListExpression : Parser State InnerExpression
emptyListExpression =
    ListExpr []
        <$ (string "["
                *> maybe
                    (choice
                        [ moreThanIndentWhitespace
                        , exactIndentWhitespace
                        , trimmed commentSequence
                        ]
                    )
                *> string "]"
           )



-- recordExpression


recordExpressionField : Parser State ( String, Expression )
recordExpressionField =
    lazy
        (\() ->
            succeed (,)
                <*> functionName
                <*> (maybe moreThanIndentWhitespace
                        *> string "="
                        *> maybe moreThanIndentWhitespace
                        *> expression
                    )
        )


recordFields : Bool -> Parser State (List ( String, Expression ))
recordFields oneOrMore =
    let
        p =
            if oneOrMore then
                sepBy1
            else
                sepBy
    in
    p (string ",") (trimmed recordExpressionField)


recordExpression : Parser State InnerExpression
recordExpression =
    lazy
        (\() ->
            RecordExpr
                <$> between (string "{")
                        (string "}")
                        (recordFields False)
        )


recordUpdateExpression : Parser State InnerExpression
recordUpdateExpression =
    lazy
        (\() ->
            between (string "{")
                (string "}")
                (RecordUpdateExpression
                    <$> (succeed RecordUpdate
                            <*> trimmed functionName
                            <*> (string "|" *> recordFields True)
                        )
                )
        )



-- literalExpression


literalExpression : Parser State InnerExpression
literalExpression =
    Literal <$> or multiLineStringLiteral stringLiteral


charLiteralExpression : Parser State InnerExpression
charLiteralExpression =
    CharLiteral <$> characterLiteral



-- lambda


lambdaExpression : Parser State InnerExpression
lambdaExpression =
    lazy
        (\() ->
            succeed (\args expr -> Lambda args expr |> LambdaExpression)
                <*> (string "\\" *> maybe moreThanIndentWhitespace *> sepBy1 moreThanIndentWhitespace functionArgument)
                <*> (trimmed (string "->") *> expression)
        )



-- Case Expression


caseBlock : Parser State Expression
caseBlock =
    lazy (\() -> caseToken *> moreThanIndentWhitespace *> expression <* moreThanIndentWhitespace <* ofToken)


caseStatement : Parser State Case
caseStatement =
    lazy
        (\() ->
            succeed (,)
                <*> pattern
                <*> (maybe (or moreThanIndentWhitespace exactIndentWhitespace) *> string "->" *> maybe moreThanIndentWhitespace *> expression)
        )


caseStatements : Parser State Cases
caseStatements =
    lazy (\() -> sepBy1 exactIndentWhitespace caseStatement)


caseExpression : Parser State InnerExpression
caseExpression =
    lazy
        (\() ->
            CaseExpression
                <$> (succeed CaseBlock
                        <*> caseBlock
                        <*> (moreThanIndentWhitespace *> withIndentedState caseStatements)
                    )
        )



-- Let Expression


letBody : Parser State (List LetDeclaration)
letBody =
    lazy
        (\() ->
            sepBy1 exactIndentWhitespace (or letDestructuringDeclaration (LetFunction <$> function))
        )


letDestructuringDeclaration : Parser State LetDeclaration
letDestructuringDeclaration =
    lazy
        (\() ->
            succeed LetDestructuring
                <*> declarablePattern
                <*> (moreThanIndentWhitespace *> string "=" *> moreThanIndentWhitespace *> expression)
        )


letBlock : Parser State (List LetDeclaration)
letBlock =
    lazy
        (\() ->
            (string "let" *> moreThanIndentWhitespace)
                *> withIndentedState letBody
                <* (choice
                        [ unstrictIndentWhitespace
                        , List.singleton <$> manySpaces
                        ]
                        *> string "in"
                   )
        )


letExpression : Parser State InnerExpression
letExpression =
    lazy
        (\() ->
            succeed (\decls -> LetBlock decls >> LetExpression)
                <*> withIndentedState2 letBlock
                <*> (moreThanIndentWhitespace *> expression)
        )


integerExpression : Parser State InnerExpression
integerExpression =
    Integer <$> Combine.Num.int


floatableExpression : Parser State InnerExpression
floatableExpression =
    Floatable <$> Combine.Num.float


ifBlockExpression : Parser State InnerExpression
ifBlockExpression =
    ifToken
        *> lazy
            (\() ->
                succeed IfBlock
                    <*> trimmed expression
                    <*> (thenToken *> trimmed expression)
                    <*> (elseToken *> moreThanIndentWhitespace *> expression)
            )


prefixOperatorExpression : Parser State InnerExpression
prefixOperatorExpression =
    PrefixOperator <$> parens prefixOperatorToken


negationExpression : Parser State InnerExpression
negationExpression =
    lazy
        (\() ->
            Negation
                <$> (string "-"
                        *> (rangedExpression
                                (choice
                                    [ qualifiedExpression
                                    , functionOrValueExpression
                                    , integerExpression
                                    , floatableExpression
                                    , tupledExpression
                                    ]
                                )
                                >>= liftRecordAccess
                           )
                    )
        )


operatorExpression : Parser State InnerExpression
operatorExpression =
    Operator <$> infixOperatorToken


functionOrValueExpression : Parser State InnerExpression
functionOrValueExpression =
    lazy
        (\() ->
            FunctionOrValue <$> choice [ functionName, typeName ]
        )


qualifiedExpression : Parser State InnerExpression
qualifiedExpression =
    lazy
        (\() ->
            succeed QualifiedExpr
                <*> many1 (typeName <* string ".")
                <*> or functionName typeName
        )


recordAccessFunctionExpression : Parser State InnerExpression
recordAccessFunctionExpression =
    ((++) "." >> RecordAccessFunction) <$> (string "." *> functionName)


tupledExpression : Parser State InnerExpression
tupledExpression =
    lazy
        (\() ->
            (\l ->
                case l of
                    [ x ] ->
                        ParenthesizedExpression x

                    xs ->
                        TupledExpression xs
            )
                <$> parens (sepBy1 (string ",") (trimmed expression))
        )
