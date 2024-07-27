module Elm.Parser.Tokens exposing
    ( asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, thenToken, aliasToken
    , dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
    , minus, minusSymbols
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, typeName
    )

{-|

@docs asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, thenToken, aliasToken

@docs dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
@docs minus, minusSymbols
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, typeName

-}

import Char
import Hex
import Parser exposing ((|.), Step(..))
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
    Parser.symbol "port"


moduleToken : Parser.Parser ()
moduleToken =
    Parser.symbol "module"


whereToken : Parser.Parser ()
whereToken =
    Parser.symbol "where"


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
    Parser.symbol "if"


thenToken : Parser.Parser ()
thenToken =
    Parser.symbol "then"


elseToken : Parser.Parser ()
elseToken =
    Parser.symbol "else"


caseToken : Parser.Parser ()
caseToken =
    Parser.symbol "case"


ofToken : Parser.Parser ()
ofToken =
    Parser.symbol "of"


letToken : Parser.Parser ()
letToken =
    Parser.symbol "let"


inToken : Parser.Parser ()
inToken =
    Parser.symbol "in"


aliasToken : Parser.Parser ()
aliasToken =
    Parser.symbol "alias"


escapedCharValue : Parser.Parser Char
escapedCharValue =
    Parser.oneOf
        [ Parser.map (\() -> '\'') (Parser.symbol "'")
        , Parser.map (\() -> '"') (Parser.symbol "\"")
        , Parser.map (\() -> '\n') (Parser.symbol "n")
        , Parser.map (\() -> '\t') (Parser.symbol "t")
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Parser.map (\() -> '\u{000D}') (Parser.symbol "r")
        , Parser.map (\() -> '\\') (Parser.symbol "\\")
        , (Parser.symbol "u{"
            |> Parser.Extra.continueWith
                (Parser.map
                    (\hex ->
                        case String.toLower hex |> Hex.fromString of
                            Ok n ->
                                Char.fromCode n

                            Err _ ->
                                '\u{0000}'
                    )
                    (Parser.variable
                        { inner = Char.isHexDigit
                        , reserved = Set.empty
                        , start = Char.isHexDigit
                        }
                    )
                )
          )
            |. Parser.symbol "}"
        ]


slashEscapedCharValue : Parser.Parser Char
slashEscapedCharValue =
    Parser.symbol "\\"
        |> Parser.Extra.continueWith escapedCharValue


characterLiteral : Parser.Parser Char
characterLiteral =
    (Parser.symbol "'"
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ slashEscapedCharValue
                , Parser.Extra.anyChar
                ]
            )
    )
        |. Parser.symbol "'"


singleOrTripleQuotedStringLiteral : Parser.Parser String
singleOrTripleQuotedStringLiteral =
    doubleQuote
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ twoDoubleQuotes
                    |> Parser.Extra.continueWith (Parser.loop "" tripleQuotedStringLiteralStep)
                , Parser.loop "" stringLiteralHelper
                ]
            )


doubleQuote : Parser.Parser ()
doubleQuote =
    Parser.symbol "\""


stringLiteralHelper : String -> Parser.Parser (Step String String)
stringLiteralHelper stringSoFar =
    Parser.oneOf
        [ doubleQuote |> Parser.map (\() -> Done stringSoFar)
        , backSlash
            |> Parser.Extra.continueWith
                (Parser.map (\v -> Loop (stringSoFar ++ String.fromChar v ++ ""))
                    escapedCharValue
                )
        , Parser.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            chompWhileIsInsideString
        ]


chompWhileIsInsideString : Parser.Parser ()
chompWhileIsInsideString =
    Parser.chompWhile (\c -> c /= '"' && c /= '\\')


twoDoubleQuotes : Parser.Parser ()
twoDoubleQuotes =
    Parser.symbol "\"\""


tripleDoubleQuote : Parser.Parser ()
tripleDoubleQuote =
    Parser.symbol "\"\"\""


tripleQuotedStringLiteralStep : String -> Parser.Parser (Step String String)
tripleQuotedStringLiteralStep stringSoFar =
    Parser.oneOf
        [ tripleDoubleQuote
            |> Parser.map (\() -> Done stringSoFar)
        , doubleQuote
            |> Parser.map (\() -> Loop (stringSoFar ++ "\""))
        , backSlash
            |> Parser.Extra.continueWith
                (Parser.map (\v -> Loop (stringSoFar ++ String.fromChar v ++ ""))
                    escapedCharValue
                )
        , Parser.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            chompWhileIsInsideString
        ]


functionName : Parser.Parser String
functionName =
    Parser.variable
        { inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = reservedList
        , start = \c -> Char.isLower c || Unicode.isLower c
        }


typeName : Parser.Parser String
typeName =
    Parser.variable
        { inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
        , start = \c -> Char.isUpper c || Unicode.isUpper c
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


prefixOperatorToken : Parser.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> Parser.symbol token |> Parser.map (\() -> token))
        |> Parser.oneOf


minus : Parser.Parser ()
minus =
    Parser.symbol "-"


minusSymbols : Parser.Parser ()
minusSymbols =
    Parser.oneOf
        [ Parser.symbol "- "
        , Parser.symbol "-\n"
        , Parser.symbol "-\u{000D}"
        ]


dot : Parser.Parser ()
dot =
    Parser.symbol "."


dotDot : Parser.Parser ()
dotDot =
    Parser.symbol ".."


squareStart : Parser.Parser ()
squareStart =
    Parser.symbol "["


squareEnd : Parser.Parser ()
squareEnd =
    Parser.symbol "]"


curlyStart : Parser.Parser ()
curlyStart =
    Parser.symbol "{"


curlyEnd : Parser.Parser ()
curlyEnd =
    Parser.symbol "}"


pipe : Parser.Parser ()
pipe =
    Parser.symbol "|"


backSlash : Parser.Parser ()
backSlash =
    Parser.symbol "\\"


arrowRight : Parser.Parser ()
arrowRight =
    Parser.symbol "->"


equal : Parser.Parser ()
equal =
    Parser.symbol "="


comma : Parser.Parser ()
comma =
    Parser.symbol ","


parensStart : Parser.Parser ()
parensStart =
    Parser.symbol "("


parensEnd : Parser.Parser ()
parensEnd =
    Parser.symbol ")"


colon : Parser.Parser ()
colon =
    Parser.symbol ":"


cons : Parser.Parser ()
cons =
    Parser.symbol "::"
