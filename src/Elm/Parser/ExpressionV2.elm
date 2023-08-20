module Elm.Parser.ExpressionV2 exposing (Problem, deadEndToString, expression)

import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Expression as Expression exposing (Expression(..), StringLiteralType(..))
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location)
import Hex
import Parser.Advanced as Parser exposing ((|.), (|=), Parser(..))
import Pratt.Advanced as Pratt
import Unicode


type Problem
    = P
    | Expected ExpectedSymbol
    | Explanation String


type ExpectedSymbol
    = IfSymbol
    | ThenSymbol
    | ElseSymbol
    | DotSymbol
    | EqualsSymbol
    | PipeSymbol


deadEndToString : Parser.DeadEnd c Problem -> String
deadEndToString deadEnd =
    "("
        ++ String.fromInt deadEnd.row
        ++ ","
        ++ String.fromInt deadEnd.col
        ++ ") "
        ++ problemToString deadEnd.problem


problemToString : Problem -> String
problemToString problem =
    case problem of
        P ->
            "No explanation"

        Expected expectedSymbol ->
            "Expected to see a " ++ expectedSymbolToString expectedSymbol

        Explanation explanation ->
            explanation


expectedSymbolToString : ExpectedSymbol -> String
expectedSymbolToString expectedSymbol =
    case expectedSymbol of
        IfSymbol ->
            "if"

        ThenSymbol ->
            "then"

        ElseSymbol ->
            "else"

        DotSymbol ->
            "."

        EqualsSymbol ->
            "="

        PipeSymbol ->
            "|"


expression : Parser c Problem (Node Expression)
expression =
    -- TODO Disabling this as this makes if expression parsing fail.
    --expressionNotApplication
    --    |> Parser.andThen
    --        (\maybeFunction ->
    --            let
    --                promoter : List (Node Expression) -> Parser c Problem (Node Expression)
    --                promoter argumentsSoFar =
    --                    Parser.oneOf
    --                        [ expressionNotApplication
    --                            |> Parser.andThen (\next -> promoter (next :: argumentsSoFar))
    --                        , Parser.succeed (complete argumentsSoFar)
    --                        ]
    --
    --                complete : List (Node Expression) -> Node Expression
    --                complete argumentsSoFar =
    --                    case argumentsSoFar of
    --                        [] ->
    --                            maybeFunction
    --
    --                        (Node lastArgumentRange _) :: _ ->
    --                            Node
    --                                { start = (Node.range maybeFunction).start, end = lastArgumentRange.end }
    --                                (FunctionCall maybeFunction (List.reverse argumentsSoFar))
    --            in
    --            promoter []
    --        )
    expressionNotApplication


expressionNotApplication : Parser c Problem (Node Expression)
expressionNotApplication =
    Pratt.expression
        { oneOf =
            [ recordAccessFunction
                |> Pratt.literal
            , digits
                |> node
                |> Pratt.literal
            , reference
                |> node
                |> Pratt.literal
            , multiLineStringLiteral
                |> Pratt.literal
            , stringLiteral
                |> Pratt.literal
            , quotedSingleQuote
                |> Pratt.literal
            , parenthesizedExpression
            , listLiteral
            , recordExpression
            , ifExpression
            ]
        , andThenOneOf =
            [ infixRight 0 "<|"
            , infixLeft 0 "|>"
            , functionCall
            , infixRight 2 "||"
            , infixRight 3 "&&"

            --, -- infix non   4 (==) = eq
            --  { direction = Non
            --  , precedence = 4
            --  , operator = "=="
            --  }
            --, -- infix non   4 (/=) = neq
            --  { direction = Non
            --  , precedence = 4
            --  , operator = "/="
            --  }
            --, -- infix non   4 (<)  = lt
            --  { direction = Non
            --  , precedence = 4
            --  , operator = "<"
            --  }
            --, -- infix non   4 (>)  = gt
            --  { direction = Non
            --  , precedence = 4
            --  , operator = ">"
            --  }
            --, -- infix non   4 (<=) = le
            --  { direction = Non
            --  , precedence = 4
            --  , operator = "<="
            --  }
            --, -- infix non   4 (>=) = ge
            --  { direction = Non
            --  , precedence = 4
            --  , operator = ">="
            --  }
            , infixRight 5 "++"
            , infixLeft 6 "+"
            , infixLeft 6 "-"
            , infixLeft 7 "*"
            , infixLeft 7 "/"
            , infixLeft 7 "//"
            , infixRight 8 "^"
            , infixLeft 9 "<<"
            , infixRight 9 ">>"
            , infixRight 5 "::"
            , infixRight 7 "</>"
            , infixLeft 8 "<?>"
            , infixLeft 5 "|="
            , infixLeft 6 "|."
            ]
        , spaces = Parser.spaces
        }


infixLeft : Int -> String -> Pratt.Config c Problem (Node Expression) -> ( Int, Node Expression -> Parser c Problem (Node Expression) )
infixLeft precedence symbol =
    Pratt.infixLeft precedence
        (Parser.symbol (Parser.Token symbol P))
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (Operation symbol Infix.Left left right)
        )


infixRight : Int -> String -> Pratt.Config c Problem (Node Expression) -> ( Int, Node Expression -> Parser c Problem (Node Expression) )
infixRight precedence symbol =
    Pratt.infixRight precedence
        (Parser.symbol (Parser.Token symbol P))
        (\left right ->
            Node
                { start = (Node.range left).start, end = (Node.range right).end }
                (Operation symbol Infix.Right left right)
        )


functionCall : Pratt.Config c Problem (Node Expression) -> ( Int, Node Expression -> Parser c Problem (Node Expression) )
functionCall =
    -- TODO Does not work yet, seems to discard the right part
    Pratt.infixRight 1
        (Parser.symbol (Parser.Token " " P))
        (\((Node leftRange leftValue) as left) right ->
            case leftValue of
                Expression.FunctionCall fn args ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.FunctionCall fn (args ++ [ right ]))

                _ ->
                    Node
                        { start = leftRange.start, end = (Node.range right).end }
                        (Expression.FunctionCall left [ right ])
        )


type alias MultilineStringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    , counter : Int
    }


reference : Parser c Problem Expression
reference =
    let
        helper : ( String, List String ) -> Parser c Problem ( String, List String )
        helper ( n, xs ) =
            Parser.oneOf
                [ Parser.symbol (Parser.Token "." P)
                    |> Parser.andThen
                        (\() ->
                            Parser.oneOf
                                [ constructorOrModuleName
                                    |> Parser.andThen (\t -> helper ( t, n :: xs ))
                                , functionName
                                    |> Parser.map (\t -> ( t, n :: xs ))
                                ]
                        )
                , Parser.succeed ( n, xs )
                ]
    in
    Parser.oneOf
        [ constructorOrModuleName
            |> Parser.andThen (\t -> helper ( t, [] ))
            |> Parser.map (\( name, moduleName ) -> FunctionOrValue moduleName name)
        , functionName
            |> Parser.map (\name -> FunctionOrValue [] name)
        ]


constructorOrModuleName : Parser c Problem String
constructorOrModuleName =
    Parser.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Tokens.reservedKeywords
        , expecting = P
        }


functionName : Parser c Problem String
functionName =
    Parser.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Tokens.reservedKeywords
        , expecting = P
        }


fieldName : Parser c Problem String
fieldName =
    Parser.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Tokens.reservedKeywords
        , expecting = Explanation "Expected a field name"
        }


multiLineStringLiteral : Parser c Problem (Node Expression)
multiLineStringLiteral =
    let
        helper : MultilineStringLiteralLoopState -> Parser c Problem (Parser.Step MultilineStringLiteralLoopState String)
        helper { counter, escaped, parts } =
            if escaped then
                escapedCharValue
                    |> Parser.map (\v -> Parser.Loop { counter = counter, escaped = False, parts = String.fromChar v :: parts })

            else
                Parser.oneOf
                    [ Parser.symbol (Parser.Token "\"\"\"" P)
                        |> Parser.map (\() -> Parser.Done (String.concat (List.reverse parts)))
                    , Parser.symbol (Parser.Token "\"" P)
                        |> Parser.getChompedString
                        |> Parser.map (\v -> Parser.Loop { counter = counter + 1, escaped = escaped, parts = v :: parts })
                    , Parser.symbol (Parser.Token "\\" P)
                        |> Parser.getChompedString
                        |> Parser.map (\_ -> Parser.Loop { counter = counter + 1, escaped = True, parts = parts })
                    , Parser.succeed (\start value end -> ( start, value, end ))
                        |= Parser.getOffset
                        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Parser.getOffset
                        |> Parser.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Parser.problem (Explanation "Expected a string character or a triple double quote")

                                else
                                    Parser.succeed (Parser.Loop { counter = counter + 1, escaped = escaped, parts = value :: parts })
                            )
                    ]
    in
    Parser.succeed (\s -> StringLiteral TripleQuote s)
        |. Parser.symbol (Parser.Token "\"\"\"" P)
        |= Parser.loop { escaped = False, parts = [], counter = 0 } helper
        |> node


stringLiteral : Parser c Problem (Node Expression)
stringLiteral =
    let
        helper : { escaped : Bool, parts : List String } -> Parser c Problem (Parser.Step { escaped : Bool, parts : List String } String)
        helper { escaped, parts } =
            if escaped then
                escapedCharValue
                    |> Parser.map
                        (\v ->
                            Parser.Loop { escaped = False, parts = String.fromChar v :: parts }
                        )

            else
                Parser.oneOf
                    [ Parser.symbol (Parser.Token "\"" P)
                        |> Parser.map (\() -> Parser.Done (String.concat <| List.reverse parts))
                    , Parser.getChompedString (Parser.symbol (Parser.Token "\\" P))
                        |> Parser.map (\_ -> Parser.Loop { escaped = True, parts = parts })
                    , Parser.succeed (\start value end -> ( start, value, end ))
                        |= Parser.getOffset
                        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Parser.getOffset
                        |> Parser.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Parser.problem (Explanation "Expected a string character or a double quote")

                                else
                                    Parser.succeed (Parser.Loop { escaped = escaped, parts = value :: parts })
                            )
                    ]
    in
    Parser.succeed (\s -> StringLiteral SingleQuote s)
        |. Parser.symbol (Parser.Token "\"" P)
        |= Parser.loop { escaped = False, parts = [] } helper
        |> node


quotedSingleQuote : Parser c Problem (Node Expression)
quotedSingleQuote =
    Parser.succeed CharLiteral
        |. Parser.symbol (Parser.Token "'" P)
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (Parser.Token "\\" P)
                |= escapedCharValue
            , Parser.getChompedString (Parser.chompIf (always True) P)
                |> Parser.map (\c -> c |> String.toList |> List.head |> Maybe.withDefault ' ')
            ]
        |. Parser.symbol (Parser.Token "'" P)
        |> node


escapedCharValue : Parser c Problem Char
escapedCharValue =
    Parser.oneOf
        [ Parser.succeed '\'' |. Parser.symbol (Parser.Token "'" P)
        , Parser.succeed '"' |. Parser.symbol (Parser.Token "\"" P)
        , Parser.succeed '\n' |. Parser.symbol (Parser.Token "n" P)
        , Parser.succeed '\t' |. Parser.symbol (Parser.Token "t" P)
        , -- Even though elm-format will change \r to a unicode version. When you don't use elm-format, this will not happen.
          Parser.succeed '\u{000D}' |. Parser.symbol (Parser.Token "r" P)
        , Parser.succeed '\\' |. Parser.symbol (Parser.Token "\\" P)
        , Parser.succeed (String.toLower >> Hex.fromString >> Result.withDefault 0 >> Char.fromCode)
            |. Parser.symbol (Parser.Token "u" P)
            |. Parser.symbol (Parser.Token "{" P)
            |= (Parser.chompWhile (\c -> String.any ((==) c) "0123456789ABCDEFabcdef") |> Parser.getChompedString)
            |. Parser.symbol (Parser.Token "}" P)
        ]


parenthesizedExpression : Pratt.Config c Problem (Node Expression) -> Parser c Problem (Node Expression)
parenthesizedExpression config =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "(" P)
        |= Parser.oneOf
            [ prefixOperatorParser
            , sequence
                { separator = Parser.Token "," P
                , end = Parser.Token ")" P
                , spaces = Parser.spaces
                , item = Pratt.subExpression 0 config
                }
                |> Parser.map TupleExpression
            ]
        |> node


sequence :
    { separator : Parser.Token x
    , end : Parser.Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    }
    -> Parser c x (List a)
sequence i =
    skip i.spaces <|
        sequenceEnd (Parser.token i.end) i.spaces i.item (Parser.token i.separator)


skip : Parser c x ignore -> Parser c x keep -> Parser c x keep
skip ignoreParser keepParser =
    Parser.succeed identity
        |. ignoreParser
        |= keepParser


sequenceEnd : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> Parser c x (List a)
sequenceEnd ender ws parseItem sep =
    let
        chompRest : a -> Parser c x (List a)
        chompRest item =
            Parser.loop [ item ] (sequenceEndForbidden ender ws parseItem sep)
    in
    Parser.oneOf
        [ parseItem |> Parser.andThen chompRest
        , ender |> Parser.map (\() -> [])
        ]


sequenceEndForbidden : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Parser.Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    skip ws <|
        Parser.oneOf
            [ skip sep <| skip ws <| Parser.map (\item -> Parser.Loop (item :: revItems)) parseItem
            , ender |> Parser.map (\() -> Parser.Done (List.reverse revItems))
            ]


prefixOperatorParser : Parser c Problem Expression
prefixOperatorParser =
    Parser.succeed PrefixOperator
        |= (Parser.oneOf
                [ Parser.symbol (Parser.Token "<|" P)
                , Parser.symbol (Parser.Token "|>" P)
                , Parser.symbol (Parser.Token "||" P)
                , Parser.symbol (Parser.Token "&&" P)
                , Parser.symbol (Parser.Token "==" P)
                , Parser.symbol (Parser.Token "/=" P)
                , Parser.symbol (Parser.Token "<" P)
                , Parser.symbol (Parser.Token ">" P)
                , Parser.symbol (Parser.Token "<=" P)
                , Parser.symbol (Parser.Token ">=" P)
                , Parser.symbol (Parser.Token "++" P)
                , Parser.symbol (Parser.Token "+" P)
                , Parser.symbol (Parser.Token "-" P)
                , Parser.symbol (Parser.Token "*" P)
                , Parser.symbol (Parser.Token "/" P)
                , Parser.symbol (Parser.Token "//" P)
                , Parser.symbol (Parser.Token "^" P)
                , Parser.symbol (Parser.Token "<<" P)
                , Parser.symbol (Parser.Token ">>" P)
                , Parser.symbol (Parser.Token "::" P)
                , Parser.symbol (Parser.Token "</>" P)
                , Parser.symbol (Parser.Token "<?>" P)
                , Parser.symbol (Parser.Token "|=" P)
                , Parser.symbol (Parser.Token "|." P)
                ]
                |> Parser.getChompedString
           )
        |. Parser.symbol (Parser.Token ")" P)


listLiteral : Pratt.Config c Problem (Node Expression) -> Parser c Problem (Node Expression)
listLiteral config =
    Parser.sequence
        { start = Parser.Token "[" P
        , separator = Parser.Token "," P
        , end = Parser.Token "]" P
        , spaces = Parser.spaces
        , item = Pratt.subExpression 1 config
        , trailing = Parser.Forbidden
        }
        |> Parser.map ListLiteral
        |> node


recordExpression : Pratt.Config c Problem (Node Expression) -> Parser c Problem (Node Expression)
recordExpression config =
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "{" P)
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.symbol (Parser.Token "}" P)
                |> Parser.map (\() -> Record [])
            , node functionName
                |> Parser.andThen (recordExpressionAfterFieldOrVarName config)
            ]
        |> node


recordExpressionAfterFieldOrVarName : Pratt.Config c Problem (Node Expression) -> Node String -> Parser c Problem Expression
recordExpressionAfterFieldOrVarName config fieldOrVarName =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\firstAssigmentValue restOfAssignments ->
                    let
                        firstAssigment : Node ( Node String, Node Expression )
                        firstAssigment =
                            Node
                                { start = (Node.range fieldOrVarName).start, end = (Node.range firstAssigmentValue).end }
                                ( fieldOrVarName, firstAssigmentValue )
                    in
                    Record (firstAssigment :: restOfAssignments)
                )
                |. Parser.symbol (Parser.Token "=" (Expected EqualsSymbol))
                |. Parser.spaces
                |= Pratt.subExpression 1 config
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.symbol (Parser.Token "}" P)
                        |> Parser.map (\() -> [])
                    , Parser.symbol (Parser.Token "," P)
                        |> Parser.andThen (\() -> recordAssignments config)
                    ]
            , Parser.succeed identity
                |. Parser.symbol (Parser.Token "|" (Expected PipeSymbol))
                |. Parser.spaces
                |= recordAssignments config
                |> Parser.andThen
                    (\assignments ->
                        case assignments of
                            [] ->
                                Parser.problem (Explanation "Expected to see at least one assignment in a record update.")

                            firstAssigment :: restOfAssignments ->
                                Parser.succeed (RecordUpdate fieldOrVarName firstAssigment restOfAssignments)
                    )
            ]


recordAssignments : Pratt.Config c Problem (Node Expression) -> Parser c Problem (List (Node ( Node String, Node Expression )))
recordAssignments config =
    sequence
        { separator = Parser.Token "," P
        , end = Parser.Token "}" P
        , spaces = Parser.spaces
        , item = recordAssignment config
        }


recordAssignment : Pratt.Config c Problem (Node Expression) -> Parser c Problem (Node ( Node String, Node Expression ))
recordAssignment config =
    Parser.succeed Tuple.pair
        |= node functionName
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "=" (Expected EqualsSymbol))
        |. Parser.spaces
        |= Pratt.subExpression 1 config
        |> node


recordAccessFunction : Parser c Problem (Node Expression)
recordAccessFunction =
    Parser.succeed RecordAccessFunction
        |. Parser.symbol (Parser.Token "." (Expected DotSymbol))
        |= fieldName
        |> node


ifExpression : Pratt.Config c Problem (Node Expression) -> Parser c Problem (Node Expression)
ifExpression config =
    Parser.succeed Expression.If
        |. Parser.symbol (Parser.Token "if" (Expected IfSymbol))
        |. Parser.spaces
        |= Pratt.subExpression 2 config
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "then" (Expected ThenSymbol))
        |. Parser.spaces
        |= Pratt.subExpression 2 config
        |. Parser.spaces
        |. Parser.symbol (Parser.Token "else" (Expected ElseSymbol))
        |. Parser.spaces
        |= Pratt.subExpression 2 config
        |> node


type alias StringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    }


digits : Parser c Problem Expression
digits =
    Parser.number
        { int = Ok Expression.IntegerLiteral
        , hex = Err P
        , octal = Err P
        , binary = Err P
        , float = Ok Expression.FloatLiteral
        , invalid = P
        , expecting = P
        }


log : a -> Parser c x b -> Parser c x b
log message parser =
    Parser.succeed ()
        |> Parser.andThen
            (\() ->
                let
                    _ =
                        Debug.log "starting" message
                in
                Parser.succeed
                    (\source offsetBefore parseResult offsetAfter ->
                        let
                            _ =
                                Debug.log "-----------------------------------------------" message

                            _ =
                                Debug.log "source         " source

                            _ =
                                Debug.log "chomped string " (String.slice offsetBefore offsetAfter source)

                            _ =
                                Debug.log "parsed result  " parseResult
                        in
                        parseResult
                    )
                    |= Parser.getSource
                    |= Parser.getOffset
                    |= parser
                    |= Parser.getOffset
            )


node : Parser c x a -> Parser c x (Node a)
node parser =
    Parser.succeed (\start a end -> Node { start = toLocation start, end = toLocation end } a)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


toLocation : ( Int, Int ) -> Location
toLocation ( line, column ) =
    { row = line, column = column }
