module Elm.Parser.Tokens exposing
    ( asToken
    , caseToken
    , characterLiteral
    , elseToken
    , exposingToken
    , functionName
    , functionNameCore
    , ifToken
    , importToken
    , inToken
    , letToken
    , moduleToken
    , multiLineStringLiteral
    , ofToken
    , portToken
    , prefixOperatorToken
    , prefixOperatorTokenCore
    , stringLiteral
    , thenToken
    , typeName
    , typeNameCore
    )

import Char
import Combine exposing (Parser, symbol)
import Combine.Char exposing (anyChar, char)
import Hex
import Parser as Core exposing ((|.), (|=), Step(..))
import Set
import Unicode


reservedList : List String
reservedList =
    [ "module"
    , "exposing"
    , "import"
    , "as"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "case"
    , "of"
    , "port"

    --, "infixr"
    --, "infixl"
    , "type"

    --, "infix" Apparently this is not a reserved keyword
    --, "alias" Apparently this is not a reserved keyword
    , "where"
    ]


portToken : Parser s ()
portToken =
    symbol "port"


moduleToken : Parser s ()
moduleToken =
    symbol "module"


exposingToken : Parser s ()
exposingToken =
    symbol "exposing"


importToken : Parser s ()
importToken =
    Combine.fromCore (Core.keyword "import")


asToken : Parser s ()
asToken =
    Combine.fromCore (Core.keyword "as")


ifToken : Parser s ()
ifToken =
    symbol "if"


thenToken : Parser s ()
thenToken =
    symbol "then"


elseToken : Parser s ()
elseToken =
    symbol "else"


caseToken : Parser s ()
caseToken =
    symbol "case"


ofToken : Parser s ()
ofToken =
    symbol "of"


letToken : Parser s ()
letToken =
    symbol "let"


inToken : Parser s ()
inToken =
    symbol "in"


escapedCharValue : Core.Parser Char
escapedCharValue =
    Core.oneOf
        [ Core.succeed '\'' |. Core.symbol "'"
        , Core.succeed '"' |. Core.symbol "\""
        , Core.succeed '\n' |. Core.symbol "n"
        , Core.succeed '\t' |. Core.symbol "t"
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.succeed '\u{000D}' |. Core.symbol "r"
        , Core.succeed '\\' |. Core.symbol "\\"
        , Core.succeed (String.toLower >> Hex.fromString >> Result.withDefault 0 >> Char.fromCode)
            |. Core.symbol "u"
            |. Core.symbol "{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


quotedSingleQuote : Parser s Char
quotedSingleQuote =
    Core.succeed (String.toList >> List.head >> Maybe.withDefault ' ')
        |. Core.symbol "'"
        |= Core.oneOf
            [ Core.succeed String.fromChar
                |. Core.symbol "\\"
                |= escapedCharValue
            , Core.getChompedString (Core.chompIf (always True))
            ]
        |. Core.symbol "'"
        |> Combine.fromCore


characterLiteral : Parser s Char
characterLiteral =
    Combine.oneOf
        [ quotedSingleQuote
        , char '\''
            |> Combine.continueWith anyChar
            |> Combine.ignore (char '\'')
        ]


type alias StringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    }


stringLiteral : Parser s String
stringLiteral =
    let
        helper : StringLiteralLoopState -> Core.Parser (Step StringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map
                        (\v ->
                            Loop { escaped = False, parts = String.fromChar v :: s.parts }
                        )

            else
                Core.oneOf
                    [ Core.symbol "\"" |> Core.map (\_ -> Done (s.parts |> List.reverse |> String.concat))
                    , Core.symbol "\\" |> Core.map (\_ -> Loop { escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a double quote"

                                else
                                    Core.succeed (Loop { escaped = s.escaped, parts = value :: s.parts })
                            )
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop { escaped = False, parts = [] } helper
        |> Combine.fromCore


type alias MultilineStringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    , counter : Int
    }


multiLineStringLiteral : Parser s String
multiLineStringLiteral =
    let
        helper : MultilineStringLiteralLoopState -> Core.Parser (Step MultilineStringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map (\v -> Loop { counter = s.counter, escaped = False, parts = String.fromChar v :: s.parts })

            else
                Core.oneOf
                    [ Core.symbol "\"\"\""
                        |> Core.map (\_ -> Done (String.concat (List.reverse s.parts)))
                    , Core.symbol "\""
                        |> Core.mapChompedString (\v _ -> Loop { counter = s.counter + 1, escaped = s.escaped, parts = v :: s.parts })
                    , Core.symbol "\\"
                        |> Core.map (\_ -> Loop { counter = s.counter + 1, escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a triple double quote"

                                else
                                    Core.succeed (Loop { counter = s.counter + 1, escaped = s.escaped, parts = value :: s.parts })
                            )
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\"\"\""
        |= Core.loop { escaped = False, parts = [], counter = 0 } helper
        |> Combine.fromCore


functionName : Parser s String
functionName =
    Combine.fromCore functionNameCore


functionNameCore : Core.Parser String
functionNameCore =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }


typeName : Parser s String
typeName =
    Combine.fromCore typeNameCore


typeNameCore : Core.Parser String
typeNameCore =
    Core.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }


allowedOperatorTokens : List String
allowedOperatorTokens =
    [ "=="
    , "/="
    , "::"
    , "++"
    , "+"
    , "*"
    , "<|"
    , "|>"
    , "||"
    , "<="
    , ">="
    , "|="
    , "|."
    , "//"
    , "</>"
    , "<?>"
    , "^"
    , "<<"
    , ">>"
    , "<"
    , ">"
    , "/"
    , "&&"
    , "-"
    ]


prefixOperatorToken : Parser s String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map Combine.string
        |> Combine.oneOf


prefixOperatorTokenCore : Core.Parser String
prefixOperatorTokenCore =
    allowedOperatorTokens
        |> List.map (\token -> Core.token token |> Core.getChompedString)
        |> Core.oneOf
