module Elm.Parser.Expression exposing (expression, failIfDifferentFrom, functionSignatureFromVarPointer)

import Combine exposing (Parser, many, maybe, modifyState, oneOf, sepBy1, string, succeed, withLocation)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns exposing (pattern)
import Elm.Parser.State as State exposing (State, popIndent, pushColumn)
import Elm.Parser.Tokens as Tokens exposing (caseToken, characterLiteral, elseToken, functionName, ifToken, multiLineStringLiteral, ofToken, prefixOperatorToken, stringLiteral, thenToken)
import Elm.Parser.TypeAnnotation exposing (typeAnnotation)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Expression as Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing ((|=), Nestable(..))
import Pratt exposing (Config)



--expressionNotApplication : Parser State (Node Expression)
--expressionNotApplication =
--    lazy
--        (\() ->
--            oneOf
--                [ numberExpression
--                , referenceExpression
--                , ifBlockExpression
--                , tupledExpression
--                , recordAccessFunctionExpression
--                , operatorExpression
--                , letExpression
--                , lambdaExpression
--                , literalExpression
--                , charLiteralExpression
--                , recordExpression
--                , glslExpression
--                , listExpression
--                , caseExpression
--                ]
--                |> Combine.andThen liftRecordAccess
--        )


expression : Parser State (Node Expression)
expression =
    Pratt.expression
        { oneOf =
            [ numberExpression
                |> Pratt.literal
            , referenceExpression
                |> Pratt.literal
            , ifBlockExpression
            , tupledExpression
            , recordAccessFunctionExpression
                |> Pratt.literal
            , negationOperation
            , letExpression
            , lambdaExpression
            , literalExpression
                |> Pratt.literal
            , charLiteralExpression
                |> Pratt.literal
            , recordExpression
            , glslExpression
                |> Pratt.literal
            , listExpression
            , caseExpression
            ]
        , andThenOneOf =
            [ infixRight 0 "<|"
            , infixLeft 0 "|>"
            , infixRight 2 "||"
            , infixRight 3 "&&"

            -- TODO Report a syntax error when encountering multiple of the comparison operators
            -- `a < b < c` is not valid Elm syntax
            -- TODO Add tests for all operators
            , infixNonAssociative 4 "=="
            , infixNonAssociative 4 "/="
            , infixNonAssociative 4 "<"
            , infixNonAssociative 4 ">"
            , infixNonAssociative 4 "<="
            , infixNonAssociative 4 ">="
            , infixRight 5 "++"
            , infixRight 5 "::"
            , infixLeft 5 "|="
            , infixLeft 6 "|."
            , infixLeft 6 "+"
            , infixLeft 6 "-"
            , infixLeft 7 "*"
            , infixLeft 7 "/"
            , infixLeft 7 "//"
            , infixRight 7 "</>"
            , infixLeft 8 "<?>"
            , infixRight 8 "^"
            , infixLeft 9 "<<"
            , infixRight 9 ">>"
            , recordAccess
            , functionCall
            ]
        , spaces = oneOf [ Layout.layout, manySpaces ]
        }


infixLeft : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixLeft precedence symbol =
    Pratt.infixLeft precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Left left right)
        )


infixNonAssociative : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixNonAssociative precedence symbol =
    Pratt.infixLeft precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Non left right)
        )


infixRight : Int -> String -> Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
infixRight precedence symbol =
    Pratt.infixRight precedence
        (Combine.symbol symbol)
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (OperatorApplication symbol Infix.Right left right)
        )


recordAccess : Config State (Node Expression) -> ( Int, Node Expression -> Parser State (Node Expression) )
recordAccess =
    Pratt.recordAccessPostfix 98
        recordAccessParser
        (\((Node leftRange _) as left) ((Node rightRange _) as field) ->
            Node
                { start = leftRange.start, end = rightRange.end }
                (Expression.RecordAccess left field)
        )


recordAccessParser : Parser State (Node String)
recordAccessParser =
    Core.succeed (\offset source -> String.slice (offset - 1) offset source)
        |= Core.getOffset
        |= Core.getSource
        |> Combine.fromCore
        |> Combine.andThen
            (\c ->
                if c == " " || c == "\n" || c == "\u{000D}" then
                    Combine.fail "Record access can't start with a space"

                else
                    string "."
                        |> Combine.continueWith (Node.parser functionName)
            )


functionCall : Pratt.Config s (Node Expression) -> ( Int, Node Expression -> Parser s (Node Expression) )
functionCall =
    Pratt.infixLeft 99
        (Combine.succeed ())
        (\((Node leftRange leftValue) as left) right ->
            case leftValue of
                Expression.Application args ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.Application (args ++ [ right ]))

                _ ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.Application [ left, right ])
        )


liftRecordAccess : Node Expression -> Parser State (Node Expression)
liftRecordAccess e =
    Combine.oneOf
        [ Combine.string "."
            |> Combine.continueWith (Node.parser functionName)
            |> Combine.andThen
                (\f ->
                    liftRecordAccess
                        (Node
                            { start = (Node.range e).start, end = (Node.range f).end }
                            (RecordAccess e f)
                        )
                )
        , Combine.succeed e
        ]



--expression : Parser State (Node Expression)
--expression =
--    expressionNotApplication
--        |> Combine.andThen
--            (\first ->
--                let
--                    complete : Range -> List (Node Expression) -> Parser s (Node Expression)
--                    complete lastExpressionRange rest =
--                        case rest of
--                            [] ->
--                                succeed first
--
--                            (Node _ (Operator _)) :: _ ->
--                                Combine.fail "Expression should not end with an operator"
--
--                            _ ->
--                                succeed
--                                    (Node
--                                        { start = (Node.range first).start, end = lastExpressionRange.end }
--                                        (Application (first :: List.reverse rest))
--                                    )
--
--                    promoter : Range -> List (Node Expression) -> Parser State (Node Expression)
--                    promoter lastExpressionRange rest =
--                        Layout.optimisticLayoutWith
--                            (\() -> complete lastExpressionRange rest)
--                            (\() ->
--                                Combine.oneOf
--                                    [ expressionNotApplication
--                                        |> Combine.andThen (\next -> promoter (Node.range next) (next :: rest))
--                                    , Combine.succeed ()
--                                        |> Combine.andThen (\() -> complete lastExpressionRange rest)
--                                    ]
--                            )
--                in
--                case first of
--                    Node _ (Operator _) ->
--                        Combine.fail "Expression should not start with an operator"
--
--                    _ ->
--                        promoter (Node.range first) []
--            )


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


listExpression : Config State (Node Expression) -> Parser State (Node Expression)
listExpression config =
    Combine.succeed ListExpr
        |> Combine.ignore (string "[")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep
            (Combine.sepBy
                (string ",")
                (Pratt.subExpression 0 config)
            )
        |> Combine.ignore (string "]")
        |> Node.parser



-- recordExpression


recordExpression : Config State (Node Expression) -> Parser State (Node Expression)
recordExpression config =
    string "{"
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ string "}" |> Combine.map (always (RecordExpr []))
                , recordContents config
                ]
            )
        |> Node.parser


recordContents : Config State (Node Expression) -> Parser State Expression
recordContents config =
    Node.parser functionName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser config fname
                    , string "="
                        |> Combine.continueWith (Pratt.subExpression 0 config)
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
                                    , Combine.succeed (\fieldUpdates -> RecordExpr (fieldUpdate :: fieldUpdates))
                                        |> Combine.ignore (string ",")
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.keep (recordFields config)
                                        |> Combine.ignore (string "}")
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Config State (Node Expression) -> Node String -> Parser State Expression
recordUpdateSyntaxParser config fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignore (string "|")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep (recordFields config)
        |> Combine.ignore (string "}")


recordFields : Config State (Node Expression) -> Parser State (List (Node RecordSetter))
recordFields config =
    succeed (::)
        |> Combine.keep (recordField config)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep
            (many
                (string ","
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith (recordField config)
                    |> Combine.ignore (maybe Layout.layout)
                )
            )


recordField : Config State (Node Expression) -> Parser State (Node RecordSetter)
recordField config =
    Node.parser
        (succeed Tuple.pair
            |> Combine.keep (Node.parser functionName)
            |> Combine.ignore (maybe Layout.layout)
            |> Combine.ignore (string "=")
            |> Combine.keep (Pratt.subExpression 0 config)
        )


literalExpression : Parser State (Node Expression)
literalExpression =
    Combine.oneOf
        [ multiLineStringLiteral
        , stringLiteral
        ]
        |> Combine.map Literal
        |> Node.parser


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Node.parser (Combine.map CharLiteral characterLiteral)



-- lambda


lambdaExpression : Config State (Node Expression) -> Parser State (Node Expression)
lambdaExpression config =
    succeed
        (\(Node { start } _) args expr ->
            Lambda args expr
                |> LambdaExpression
                |> Node { start = start, end = (Node.range expr).end }
        )
        |> Combine.keep (Node.parser (string "\\"))
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep (sepBy1 (maybe Layout.layout) pattern)
        |> Combine.ignore (Layout.maybeAroundBothSides (string "->"))
        |> Combine.keep (Pratt.subExpression 0 config)



-- Case Expression


caseStatement : Config State (Node Expression) -> Parser State Case
caseStatement config =
    Combine.succeed Tuple.pair
        |> Combine.keep pattern
        |> Combine.ignore (maybe (Combine.oneOf [ Layout.layout, Layout.layoutStrict ]))
        |> Combine.ignore (string "->")
        |> Combine.keep (Pratt.subExpression 0 config)


caseStatements : Config State (Node Expression) -> Parser State ( Location, Cases )
caseStatements config =
    Combine.many1WithEndLocationForLastElement
        (\( _, case_ ) -> Node.range case_)
        (caseStatementWithCorrectIndentation config)


caseStatementWithCorrectIndentation : Config State (Node Expression) -> Parser State Case
caseStatementWithCorrectIndentation config =
    Combine.withState
        (\s ->
            Combine.withLocation
                (\l ->
                    if State.expectedColumn s == l.column then
                        caseStatement config

                    else
                        Combine.fail "Indentation is incorrect to be a case statement"
                )
        )


caseExpression : Config State (Node Expression) -> Parser State (Node Expression)
caseExpression config =
    Combine.succeed
        (\caseKeyword caseBlock_ ( end, cases ) ->
            Node { start = (Node.range caseKeyword).start, end = end }
                (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keep (Node.parser caseToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState (caseStatements config))



-- Let Expression


letBody : Config State (Node Expression) -> Parser State (List (Node LetDeclaration))
letBody config =
    -- TODO Add failing tests where let declarations are not aligned
    Combine.succeed (::)
        |> Combine.keep (blockElement config)
        |> Combine.keep (many (blockElement config |> Combine.ignore (maybe Layout.layout)))


blockElement : Config State (Node Expression) -> Parser State (Node LetDeclaration)
blockElement config =
    pattern
        |> Combine.andThen
            (\(Node r p) ->
                case p of
                    Pattern.VarPattern v ->
                        functionWithNameNode config (Node r v)
                            |> Combine.map (\fn -> Node (Expression.functionRange fn) (LetFunction fn))

                    _ ->
                        letDestructuringDeclarationWithPattern config (Node r p)
            )


letDestructuringDeclarationWithPattern : Config State (Node Expression) -> Node Pattern -> Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern config pattern =
    succeed
        (\expr ->
            Node { start = (Node.range pattern).start, end = (Node.range expr).end } (LetDestructuring pattern expr)
        )
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore (string "=")
        |> Combine.keep (Pratt.subExpression 0 config)


letExpression : Config State (Node Expression) -> Parser State (Node Expression)
letExpression config =
    succeed (\(Node { start } _) decls expr -> Node { start = start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression))
        |> Combine.keep (Node.parser (string "let"))
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState (letBody config))
        |> Combine.ignore (oneOf [ Layout.layout, manySpaces ])
        |> Combine.ignore (string "in")
        |> Combine.ignore Layout.layout
        |> Combine.keep (Pratt.subExpression 0 config)


numberExpression : Parser State (Node Expression)
numberExpression =
    Node.parser (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Config State (Node Expression) -> Parser State (Node Expression)
ifBlockExpression config =
    Combine.succeed
        (\(Node { start } _) condition ifTrue ifFalse ->
            Node
                { start = start, end = (Node.range ifFalse).end }
                (IfBlock condition ifTrue ifFalse)
        )
        |> Combine.keep (Node.parser ifToken)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore thenToken
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep (Pratt.subExpression 0 config)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (Pratt.subExpression 0 config)


negationOperation : Config s (Node Expression) -> Parser s (Node Expression)
negationOperation =
    Pratt.prefix 3
        (Combine.symbol "-")
        (\((Node { start, end } _) as subExpr) ->
            Node
                { start = { row = start.row, column = start.column - 1 }, end = end }
                (Negation subExpr)
        )


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ModuleName -> String -> Parser s Expression
        helper moduleNameSoFar nameOrSegment =
            Combine.oneOf
                [ string "."
                    |> Combine.continueWith
                        (Combine.oneOf
                            [ Tokens.typeName
                                |> Combine.andThen (\t -> helper (nameOrSegment :: moduleNameSoFar) t)
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
            |> Combine.andThen (\t -> helper [] t)
        , Tokens.functionName
            |> Combine.map (\v -> FunctionOrValue [] v)
        ]
        |> Node.parser


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Combine.succeed (\field -> RecordAccessFunction ("." ++ field))
        |> Combine.ignore (string ".")
        |> Combine.keep functionName
        |> Node.parser


tupledExpression : Config State (Node Expression) -> Parser State (Node Expression)
tupledExpression config =
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
                    |> Combine.continueWith (Pratt.subExpression 0 config)
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.keep (Pratt.subExpression 0 config)
                |> Combine.keep commaSep

        closingParen : Parser state ()
        closingParen =
            Combine.symbol ")"
    in
    Combine.symbol "("
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
        |> Node.parser


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    withLocation
        (\location ->
            modifyState (pushColumn location.column)
                |> Combine.continueWith p
                |> Combine.ignore (modifyState popIndent)
        )


functionWithNameNode : Config State (Node Expression) -> Node String -> Parser State Function
functionWithNameNode config pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            succeed (\args expr -> Node { start = (Node.range varPointer).start, end = (Node.range expr).end } (FunctionImplementation varPointer args expr))
                |> Combine.keep (many (pattern |> Combine.ignore (maybe Layout.layout)))
                |> Combine.ignore (string "=")
                |> Combine.keep (Pratt.subExpression 0 config)

        fromParts : Node Signature -> Node FunctionImplementation -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.ignore (maybe Layout.layoutStrict)
                |> Combine.andThen
                    (\sig ->
                        Node.parser functionName
                            |> Combine.andThen (failIfDifferentFrom varPointer)
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


failIfDifferentFrom : Node String -> Node String -> Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.fail <| "Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (string ":")
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.keep typeAnnotation
