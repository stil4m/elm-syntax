module Elm.Parser.Tokens exposing
    ( inToken
    , equal, parensEnd
    , minusFollowedBySingleWhitespaceFollowedBy
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode
    )

{-|

@docs inToken

@docs equal, parensEnd
@docs minusFollowedBySingleWhitespaceFollowedBy
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode

-}

import Char
import Char.Extra
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import ParserFast
import Set exposing (Set)


reservedList : Set String
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
        |> Set.fromList


inToken : ParserFast.Parser ()
inToken =
    ParserFast.keyword "in" ()


escapedCharValueMap : (Char -> res) -> ParserFast.Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        { exponent = 0, result = 0 }
        string
        |> .result


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


slashEscapedCharValue : ParserFast.Parser Char
slashEscapedCharValue =
    ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity)


characterLiteral : ParserFast.Parser Char
characterLiteral =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2
            slashEscapedCharValue
            ParserFast.anyChar
        )
        |> ParserFast.followedBySymbol "'"


singleOrTripleQuotedStringLiteral : ParserFast.Parser String
singleOrTripleQuotedStringLiteral =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : ParserFast.Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.whileWithoutLinebreak (\c -> c /= '"' && c /= '\\'))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : ParserFast.Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while (\c -> c /= '"' && c /= '\\'))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


functionName : ParserFast.Parser String
functionName =
    ParserFast.ifFollowedByWhileExceptWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        reservedList


functionNameNode : ParserFast.Parser (Node String)
functionNameNode =
    ParserFast.ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak
        (\start name end -> Node { start = start, end = end } name)
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        reservedList


functionNameMapWithRange : (Range -> String -> res) -> ParserFast.Parser res
functionNameMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak
        (\start name end -> rangeAndNameToResult { start = start, end = end } name)
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        reservedList


functionNameNotInfixNode : ParserFast.Parser (Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak
        (\start name end -> Node { start = start, end = end } name)
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (Set.insert "infix" reservedList)


typeName : ParserFast.Parser String
typeName =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


typeNameNode : ParserFast.Parser (Node String)
typeNameNode =
    ParserFast.ifFollowedByWhileMapWithStartAndEndPositionWithoutLinebreak
        (\start name end -> Node { start = start, end = end } name)
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


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


prefixOperatorToken : ParserFast.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> ParserFast.symbol token token)
        |> ParserFast.oneOf


minusFollowedBySingleWhitespaceFollowedBy : ParserFast.Parser res -> ParserFast.Parser res
minusFollowedBySingleWhitespaceFollowedBy next =
    ParserFast.oneOf3
        (ParserFast.symbolFollowedBy "- " next)
        (ParserFast.symbolFollowedBy "-\n" next)
        (ParserFast.symbolFollowedBy "-\u{000D}" next)


equal : ParserFast.Parser ()
equal =
    ParserFast.symbol "=" ()


parensEnd : ParserFast.Parser ()
parensEnd =
    ParserFast.symbol ")" ()
