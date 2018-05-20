module Elm.Parser.Tokens
    exposing
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

import Char exposing (fromCode)
import Combine exposing (Parser, between, choice, count, fail, many, many1, or, string, succeed)
import Combine.Char exposing (anyChar, char, oneOf)
import Dict exposing (Dict)
import Hex
import Parser as Core exposing ((|.), (|=), Nestable(..), Step(..))
import Set


reserved : Dict String Bool
reserved =
    reservedList
        |> List.map (\c -> ( c, True ))
        |> Dict.fromList


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
    , "infixr"
    , "infixl"
    , "infix"
    , "type"

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


importToken : Parser s String
importToken =
    string "import"


asToken : Parser s String
asToken =
    string "as"


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


notReserved : String -> Parser s String
notReserved match =
    if Dict.member match reserved then
        fail "functionName is reserved"

    else
        succeed match


escapedChar : Parser s Char
escapedChar =
    char '\\'
        |> Combine.continueWith
            (choice
                [ succeed '\'' |> Combine.ignore (char '\'')
                , succeed '"' |> Combine.ignore (char '"')
                , succeed '\n' |> Combine.ignore (char 'n')
                , succeed '\t' |> Combine.ignore (char 't')
                , succeed '\\' |> Combine.ignore (char '\\')
                , succeed '\u{0007}' |> Combine.ignore (char 'a')
                , succeed '\u{0008}' |> Combine.ignore (char 'b')
                , succeed '\u{000C}' |> Combine.ignore (char 'f')
                , succeed '\u{000D}' |> Combine.ignore (char 'r')
                , succeed '\u{000B}' |> Combine.ignore (char 'v')
                , (Core.succeed identity
                    |. Core.symbol "x"
                    |= Core.getChompedString
                        (Core.succeed ()
                            |. Core.chompIf Char.isHexDigit
                            |. Core.chompIf Char.isHexDigit
                        )
                  )
                    |> Core.andThen
                        (\l ->
                            case Hex.fromString <| String.toLower l of
                                Ok x ->
                                    Core.succeed (fromCode x)

                                Err x ->
                                    Core.problem x
                        )
                    |> Combine.fromCore
                ]
            )


escapedCharValue : Core.Parser Char
escapedCharValue =
    Core.oneOf
        [ Core.succeed '\'' |. Core.symbol "'"
        , Core.succeed '"' |. Core.symbol "\""
        , Core.succeed '\n' |. Core.symbol "n"
        , Core.succeed '\t' |. Core.symbol "t"
        , Core.succeed '\\' |. Core.symbol "\\"
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
        (char '\'' |> Combine.continueWith anyChar |> Combine.ignore (char '\''))


type alias StringLiteralLoopState =
    { escaped : Bool, parts : List String }


stringLiteral : Parser s String
stringLiteral =
    let
        helper : StringLiteralLoopState -> Core.Parser (Step StringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map
                        (\v ->
                            Loop { s | escaped = False, parts = String.fromList [ v ] :: s.parts }
                        )

            else
                Core.oneOf
                    [ Core.symbol "\""
                        |> Core.map (\_ -> Done (String.concat <| List.reverse s.parts))
                    , Core.getChompedString (Core.symbol "\\")
                        |> Core.map (\v -> Loop { s | escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a double quote"

                                else
                                    Core.succeed (Loop { s | parts = value :: s.parts })
                            )
                    ]
    in
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop { escaped = False, parts = [] } helper
        |> Combine.fromCore


type alias MultilineStringLiteralLoopState =
    { escaped : Bool, parts : List String, counter : Int }


multiLineStringLiteral : Parser s String
multiLineStringLiteral =
    let
        helper : MultilineStringLiteralLoopState -> Core.Parser (Step MultilineStringLiteralLoopState String)
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Core.map (\v -> Loop { s | escaped = False, parts = String.fromList [ v ] :: s.parts })

            else
                Core.oneOf
                    [ Core.symbol "\"\"\""
                        |> Core.map (\_ -> Done (String.concat s.parts))
                    , Core.symbol "\""
                        |> Core.getChompedString
                        |> Core.map (\v -> Loop { s | counter = s.counter + 1, parts = v :: s.parts })
                    , Core.symbol "\\"
                        |> Core.getChompedString
                        |> Core.map (\v -> Loop { s | counter = s.counter + 1, escaped = True, parts = s.parts })
                    , Core.succeed (\start value end -> ( start, value, end ))
                        |= Core.getOffset
                        |= Core.getChompedString (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Core.getOffset
                        |> Core.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Core.problem "Expected a string character or a triple double quote"

                                else
                                    Core.succeed (Loop { s | counter = s.counter + 1, parts = value :: s.parts })
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
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList reservedList
        }
        |> Combine.fromCore


typeName : Parser s String
typeName =
    Core.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
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
