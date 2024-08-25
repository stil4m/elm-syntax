module Elm.Parser.Tokens exposing
    ( inToken
    , equal, parensEnd
    , minusFollowedBySingleWhitespace
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode
    )

{-|

@docs inToken

@docs equal, parensEnd
@docs minusFollowedBySingleWhitespace
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNode, functionNameMapWithRange, functionNameNotInfixNode, typeName, typeNameNode

-}

import Char
import Char.Extra
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Hex
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
            (ParserFast.map
                (\hex ->
                    case String.toLower hex |> Hex.fromString of
                        Ok n ->
                            charToRes (Char.fromCode n)

                        Err _ ->
                            charToRes '\u{0000}'
                )
                (ParserFast.ifFollowedByWhileWithoutLinebreak
                    Char.isHexDigit
                    Char.isHexDigit
                )
            )
            |> ParserFast.followedBySymbol "}"
        )


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


minusFollowedBySingleWhitespace : ParserFast.Parser res -> ParserFast.Parser res
minusFollowedBySingleWhitespace next =
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
