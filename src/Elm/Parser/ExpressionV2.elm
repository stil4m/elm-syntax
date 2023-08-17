module Elm.Parser.ExpressionV2 exposing (expression)

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
    | Explanation String


expression : Parser c Problem (Node Expression)
expression =
    expressionNotApplication
        |> Parser.andThen
            (\maybeFunction ->
                let
                    promoter : List (Node Expression) -> Parser c Problem (Node Expression)
                    promoter rest =
                        Parser.oneOf
                            [ expressionNotApplication
                                |> Parser.andThen (\next -> promoter (next :: rest))
                            , complete rest
                            ]

                    complete : List (Node Expression) -> Parser c x (Node Expression)
                    complete rest =
                        case rest of
                            [] ->
                                Parser.succeed maybeFunction

                            _ ->
                                let
                                    (Node { end } _) =
                                        List.head rest |> Maybe.withDefault maybeFunction
                                in
                                Parser.succeed
                                    (Node
                                        { start = (Node.range maybeFunction).start, end = end }
                                        (FunctionCall maybeFunction (List.reverse rest))
                                    )
                in
                promoter []
            )


expressionNotApplication : Parser c Problem (Node Expression)
expressionNotApplication =
    Pratt.expression
        { oneOf =
            [ digits
                |> node
                |> Pratt.literal
            , reference
                |> node
                |> Pratt.literal
            , multiLineStringLiteral
                |> Parser.map (\s -> StringLiteral TripleQuote s)
                |> node
                |> Pratt.literal
            , stringLiteral
                |> Parser.map (\s -> StringLiteral SingleQuote s)
                |> node
                |> Pratt.literal
            , quotedSingleQuote
                |> Parser.map CharLiteral
                |> node
                |> Pratt.literal
            , parenthesizedLiteral
                |> node
                |> Pratt.literal
            , listLiteral
                |> node
                |> Pratt.literal
            ]
        , andThenOneOf =
            [ infixRight 0 "<|"
            , infixLeft 0 "|>"
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


multiLineStringLiteral : Parser c Problem String
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
                        |> Parser.map (\_ -> Parser.Done (String.concat (List.reverse parts)))
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
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\"\"\"" P)
        |= Parser.loop { escaped = False, parts = [], counter = 0 } helper


stringLiteral : Parser c Problem String
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
                        |> Parser.map (\_ -> Parser.Done (String.concat <| List.reverse parts))
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
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\"" P)
        |= Parser.loop { escaped = False, parts = [] } helper


quotedSingleQuote : Parser c Problem Char
quotedSingleQuote =
    Parser.succeed (String.toList >> List.head >> Maybe.withDefault ' ')
        |. Parser.symbol (Parser.Token "'" P)
        |= Parser.oneOf
            [ Parser.succeed (List.singleton >> String.fromList)
                |. Parser.symbol (Parser.Token "\\" P)
                |= escapedCharValue
            , Parser.getChompedString (Parser.chompIf (always True) P)
            ]
        |. Parser.symbol (Parser.Token "'" P)


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


parenthesizedLiteral : Parser c Problem Expression
parenthesizedLiteral =
    Parser.lazy
        (\() ->
            Parser.succeed identity
                |. Parser.symbol (Parser.Token "(" P)
                |= Parser.oneOf
                    [ prefixOperatorParser
                    , sequence
                        { separator = Parser.Token "," P
                        , end = Parser.Token ")" P
                        , spaces = Parser.spaces
                        , item = expression
                        }
                        |> Parser.map TupleExpression
                    ]
        )


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
        , ender |> Parser.map (\_ -> [])
        ]


sequenceEndForbidden : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Parser.Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
    skip ws <|
        Parser.oneOf
            [ skip sep <| skip ws <| Parser.map (\item -> Parser.Loop (item :: revItems)) parseItem
            , ender |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
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


listLiteral : Parser c Problem Expression
listLiteral =
    Parser.lazy
        (\() ->
            Parser.sequence
                { start = Parser.Token "[" P
                , separator = Parser.Token "," P
                , end = Parser.Token "]" P
                , spaces = Parser.spaces
                , item = expression
                , trailing = Parser.Forbidden
                }
        )
        |> Parser.map ListLiteral


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
        , float = Err P
        , invalid = P
        , expecting = P
        }


node : Parser c x a -> Parser c x (Node a)
node parser =
    Parser.succeed (\start a end -> Node { start = toLocation start, end = toLocation end } a)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


toLocation : ( Int, Int ) -> Location
toLocation ( line, column ) =
    { row = line, column = column }
