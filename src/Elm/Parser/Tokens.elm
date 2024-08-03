module Elm.Parser.Tokens exposing
    ( asToken, caseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, portToken, aliasToken
    , dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
    , minus, minusSymbols
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNotInfix, typeName
    )

{-|

@docs asToken, caseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, portToken, aliasToken

@docs squareEnd, curlyEnd, arrowRight, equal, parensEnd
@docs minusFollowedBySingleWhitespace
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNotInfix, typeName

-}

import Char
import Char.Extra
import Hex
import Parser exposing ((|.), (|=))
import Parser.Advanced
import Parser.Extra
import Set exposing (Set)
import Unicode


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


portToken : Parser.Parser ()
portToken =
    Parser.keyword "port"


moduleToken : Parser.Parser ()
moduleToken =
    Parser.keyword "module"


whereToken : Parser.Parser ()
whereToken =
    Parser.keyword "where"


exposingToken : Parser.Parser ()
exposingToken =
    Parser.symbol "exposing"


importToken : Parser.Parser ()
importToken =
    Parser.keyword "import"


asToken : Parser.Parser ()
asToken =
    Parser.keyword "as"


ifToken : Parser.Parser ()
ifToken =
    Parser.keyword "if"


caseToken : Parser.Parser ()
caseToken =
    Parser.keyword "case"


letToken : Parser.Parser ()
letToken =
    Parser.keyword "let"


inToken : Parser.Parser ()
inToken =
    ParserFast.keyword "in" ()


aliasToken : Parser.Parser ()
aliasToken =
    Parser.keyword "alias"


escapedCharValue : Parser.Parser Char
escapedCharValue =
    ParserFast.oneOf
        [ ParserFast.symbol "'" '\''
        , ParserFast.symbol "\"" '"'
        , ParserFast.symbol "n" '\n'
        , ParserFast.symbol "t" '\t'
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Parser.map (\() -> '\u{000D}') (Parser.symbol "r")
        , Parser.map (\() -> '\\') (Parser.symbol "\\")
        , Parser.map
            (\() ->
                \hex ->
                    case String.toLower hex |> Hex.fromString of
                        Ok n ->
                            Char.fromCode n

                        Err _ ->
                            '\u{0000}'
            )
            (Parser.symbol "u{")
            |= Parser.variable
                { inner = Char.isHexDigit
                , reserved = Set.empty
                , start = Char.isHexDigit
                }
            |. Parser.symbol "}"
        ]


slashEscapedCharValue : ParserFast.Parser Char
slashEscapedCharValue =
    ParserFast.symbolFollowedBy "\\" escapedCharValue


characterLiteral : ParserFast.Parser Char
characterLiteral =
    ParserFast.map2
        (\res () -> res)
        (ParserFast.symbolFollowedBy "'"
            (ParserFast.oneOf2
                slashEscapedCharValue
                ParserFast.Extra.anyChar
            )
        )
        (ParserFast.symbol "'" ())


singleOrTripleQuotedStringLiteral : ParserFast.Parser String
singleOrTripleQuotedStringLiteral =
    doubleQuote
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ twoDoubleQuotes
                    |> Parser.Extra.continueWith
                        (Parser.Advanced.loop "" tripleQuotedStringLiteralStep)
                , Parser.Advanced.loop "" stringLiteralHelper
                ]
            )
            (ParserFast.Advanced.loop "" stringLiteralHelper)
        )


doubleQuote : Parser.Parser ()
doubleQuote =
    Parser.symbol "\""


stringLiteralHelper : String -> Parser.Parser (Parser.Advanced.Step String String)
stringLiteralHelper stringSoFar =
    Parser.oneOf
        [ doubleQuote |> Parser.map (\() -> Parser.Advanced.Done stringSoFar)
        , Parser.map (\() -> \v -> Parser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ ""))
            backSlash
            |= escapedCharValue
        , Parser.mapChompedString
            (\value () -> Parser.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


chompWhileIsInsideString : ParserFast.Parser ()
chompWhileIsInsideString =
    ParserFast.chompWhile (\c -> c /= '"' && c /= '\\')


twoDoubleQuotes : Parser.Parser ()
twoDoubleQuotes =
    Parser.symbol "\"\""


tripleDoubleQuote : Parser.Parser ()
tripleDoubleQuote =
    Parser.symbol "\"\"\""


tripleQuotedStringLiteralStep : String -> Parser.Parser (Parser.Advanced.Step String String)
tripleQuotedStringLiteralStep stringSoFar =
    Parser.oneOf
        [ tripleDoubleQuote
            |> Parser.map (\() -> Parser.Advanced.Done stringSoFar)
        , doubleQuote
            |> Parser.map (\() -> Parser.Advanced.Loop (stringSoFar ++ "\""))
        , Parser.map (\() -> \v -> Parser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ ""))
            backSlash
            |= escapedCharValue
        , Parser.mapChompedString
            (\value () -> Parser.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


functionName : ParserFast.Parser String
functionName =
    ParserFast.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        , reserved = reservedList
        , start =
            \c -> Char.isLower c || Unicode.isLower c
        }


functionNameNotInfix : Parser.Parser String
functionNameNotInfix =
    Parser.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                charIsAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.insert "infix" reservedList
        , start =
            \c -> Char.isLower c || Unicode.isLower c
        }


charIsAlphaNumFast : Char -> Bool
charIsAlphaNumFast char =
    -- Char.isAlphaNum does not reuse the same Char.toCode and is therefore slightly slower
    let
        charCode : Int
        charCode =
            char |> Char.toCode
    in
    charCodeIsLower charCode || charCodeIsUpper charCode || charCodeIsDigit charCode


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


typeName : Parser.Parser String
typeName =
    Parser.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.insert "infix" reservedList
        , start =
            \c -> Char.isLower c || Unicode.isLower c
        }


typeName : ParserFast.Parser String
typeName =
    ParserFast.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        , start =
            \c -> Char.isUpper c || Unicode.isUpper c
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


prefixOperatorToken : ParserFast.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> ParserFast.symbol token token)
        |> ParserFast.oneOf


minusFollowedBySingleWhitespace : ParserFast.Parser res -> ParserFast.Parser res
minusFollowedBySingleWhitespace next =
    ParserFast.oneOf
        [ ParserFast.symbolFollowedBy "- " next
        , ParserFast.symbolFollowedBy "-\n" next
        , ParserFast.symbolFollowedBy "-\u{000D}" next
        ]


squareEnd : ParserFast.Parser ()
squareEnd =
    ParserFast.symbol "]" ()


curlyEnd : ParserFast.Parser ()
curlyEnd =
    ParserFast.symbol "}" ()


arrowRight : ParserFast.Parser ()
arrowRight =
    ParserFast.symbol "->" ()


equal : ParserFast.Parser ()
equal =
    ParserFast.symbol "=" ()


parensEnd : ParserFast.Parser ()
parensEnd =
    ParserFast.symbol ")" ()
