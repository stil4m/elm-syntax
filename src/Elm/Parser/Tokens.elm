module Elm.Parser.Tokens exposing
    ( asToken
    , caseToken
    , characterLiteral
    , elseToken
    , escapedCharValue
    , exposingToken
    , functionName
    , functionOrTypeName
    , ifToken
    , importToken
    , infixOperatorToken
    , moduleToken
    , multiLineStringLiteral
    , ofToken
    , portToken
    , prefixOperatorToken
    , stringLiteral
    , thenToken
    , typeName
    )

import Char
import Combine exposing (Parser, fail, many1, or, string, succeed)
import Combine.Char exposing (anyChar, char, oneOf)
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


portToken : Parser s String
portToken =
    string "port"


moduleToken : Parser s String
moduleToken =
    string "module"


exposingToken : Parser s String
exposingToken =
    string "exposing"


importToken : Parser s ()
importToken =
    Combine.fromCore (Core.keyword "import")


asToken : Parser s ()
asToken =
    Combine.fromCore (Core.keyword "as")


ifToken : Parser s String
ifToken =
    string "if"


thenToken : Parser s String
thenToken =
    string "then"


elseToken : Parser s String
elseToken =
    string "else"


caseToken : Parser s String
caseToken =
    string "case"


ofToken : Parser s String
ofToken =
    string "of"


functionOrTypeName : Parser s String
functionOrTypeName =
    or functionName typeName


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
            |= (Core.chompWhile (\c -> String.any ((==) c) "0123456789ABCDEFabcdef") |> Core.getChompedString)
            |. Core.symbol "}"
        ]


quotedSingleQuote : Parser s Char
quotedSingleQuote =
    Core.succeed (String.toList >> List.head >> Maybe.withDefault ' ')
        |. Core.symbol "'"
        |= Core.oneOf
            [ Core.succeed (List.singleton >> String.fromList) |. Core.symbol "\\" |= escapedCharValue
            , Core.getChompedString (Core.chompIf (always True))
            ]
        |. Core.symbol "'"
        |> Combine.fromCore


characterLiteral : Parser s Char
characterLiteral =
    or quotedSingleQuote
        (char '\''
            |> Combine.continueWith anyChar
            |> Combine.ignore (char '\'')
        )


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
                            Loop { escaped = False, parts = String.fromList [ v ] :: s.parts }
                        )

            else
                Core.oneOf
                    [ Core.symbol "\""
                        |> Core.map (\_ -> Done (String.concat <| List.reverse s.parts))
                    , Core.getChompedString (Core.symbol "\\")
                        |> Core.map (\_ -> Loop { escaped = True, parts = s.parts })
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
                        |> Core.getChompedString
                        |> Core.map (\v -> Loop { counter = s.counter + 1, escaped = s.escaped, parts = v :: s.parts })
                    , Core.symbol "\\"
                        |> Core.getChompedString
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
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }
        |> Combine.fromCore


typeName : Parser s String
typeName =
    Core.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }
        |> Combine.fromCore


excludedOperators : List String
excludedOperators =
    [ ":", "->", "--", "=" ]


allowedOperatorTokens : List Char
allowedOperatorTokens =
    [ '+', '-', ':', '/', '*', '>', '<', '=', '/', '&', '^', '%', '|', '!', '.', '#', '$', '≡', '~', '?', '@' ]


allowedPrefixOperatorTokens : List Char
allowedPrefixOperatorTokens =
    ',' :: allowedOperatorTokens


prefixOperatorToken : Parser s String
prefixOperatorToken =
    operatorTokenFromList allowedPrefixOperatorTokens


infixOperatorToken : Parser s String
infixOperatorToken =
    operatorTokenFromList allowedOperatorTokens


operatorTokenFromList : List Char -> Parser s String
operatorTokenFromList allowedChars =
    many1 (oneOf allowedChars)
        |> Combine.map String.fromList
        |> Combine.andThen
            (\m ->
                if List.member m excludedOperators then
                    fail "operator is not allowed"

                else
                    succeed m
            )
