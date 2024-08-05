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

@docs dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
@docs minus, minusSymbols
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNotInfix, typeName

-}

import Char
import CustomParser
import CustomParser.Advanced
import CustomParser.Extra
import Hex
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


portToken : CustomParser.Parser ()
portToken =
    CustomParser.keyword "port"


moduleToken : CustomParser.Parser ()
moduleToken =
    CustomParser.keyword "module"


whereToken : CustomParser.Parser ()
whereToken =
    CustomParser.keyword "where"


exposingToken : CustomParser.Parser ()
exposingToken =
    CustomParser.symbol "exposing"


importToken : CustomParser.Parser ()
importToken =
    CustomParser.keyword "import"


asToken : CustomParser.Parser ()
asToken =
    CustomParser.keyword "as"


ifToken : CustomParser.Parser ()
ifToken =
    CustomParser.keyword "if"


caseToken : CustomParser.Parser ()
caseToken =
    CustomParser.keyword "case"


letToken : CustomParser.Parser ()
letToken =
    CustomParser.keyword "let"


inToken : CustomParser.Parser ()
inToken =
    CustomParser.keyword "in"


aliasToken : CustomParser.Parser ()
aliasToken =
    CustomParser.keyword "alias"


escapedCharValue : CustomParser.Parser Char
escapedCharValue =
    CustomParser.oneOf
        [ CustomParser.map (\() -> '\'') (CustomParser.symbol "'")
        , CustomParser.map (\() -> '"') (CustomParser.symbol "\"")
        , CustomParser.map (\() -> '\n') (CustomParser.symbol "n")
        , CustomParser.map (\() -> '\t') (CustomParser.symbol "t")
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          CustomParser.map (\() -> '\u{000D}') (CustomParser.symbol "r")
        , CustomParser.map (\() -> '\\') (CustomParser.symbol "\\")
        , CustomParser.map3
            (\() hex () ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            (CustomParser.symbol "u{")
            (CustomParser.variable
                { inner = Char.isHexDigit
                , reserved = Set.empty
                , start = Char.isHexDigit
                }
            )
            (CustomParser.symbol "}")
        ]


slashEscapedCharValue : CustomParser.Parser Char
slashEscapedCharValue =
    CustomParser.symbol "\\"
        |> CustomParser.Extra.continueWith escapedCharValue


characterLiteral : CustomParser.Parser Char
characterLiteral =
    CustomParser.map3
        (\() res () -> res)
        (CustomParser.symbol "'")
        (CustomParser.oneOf
            [ slashEscapedCharValue
            , CustomParser.Extra.anyChar
            ]
        )
        (CustomParser.symbol "'")


singleOrTripleQuotedStringLiteral : CustomParser.Parser String
singleOrTripleQuotedStringLiteral =
    doubleQuote
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                [ twoDoubleQuotes
                    |> CustomParser.Extra.continueWith
                        (CustomParser.Advanced.loop "" tripleQuotedStringLiteralStep)
                , CustomParser.Advanced.loop "" stringLiteralHelper
                ]
            )


doubleQuote : CustomParser.Parser ()
doubleQuote =
    CustomParser.symbol "\""


stringLiteralHelper : String -> CustomParser.Parser (CustomParser.Advanced.Step String String)
stringLiteralHelper stringSoFar =
    CustomParser.oneOf
        [ doubleQuote |> CustomParser.map (\() -> CustomParser.Advanced.Done stringSoFar)
        , CustomParser.map2
            (\() v ->
                CustomParser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            backSlash
            escapedCharValue
        , CustomParser.mapChompedString
            (\value () -> CustomParser.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


chompWhileIsInsideString : CustomParser.Parser ()
chompWhileIsInsideString =
    CustomParser.chompWhile (\c -> c /= '"' && c /= '\\')


twoDoubleQuotes : CustomParser.Parser ()
twoDoubleQuotes =
    CustomParser.symbol "\"\""


tripleDoubleQuote : CustomParser.Parser ()
tripleDoubleQuote =
    CustomParser.symbol "\"\"\""


tripleQuotedStringLiteralStep : String -> CustomParser.Parser (CustomParser.Advanced.Step String String)
tripleQuotedStringLiteralStep stringSoFar =
    CustomParser.oneOf
        [ tripleDoubleQuote
            |> CustomParser.map (\() -> CustomParser.Advanced.Done stringSoFar)
        , doubleQuote
            |> CustomParser.map (\() -> CustomParser.Advanced.Loop (stringSoFar ++ "\""))
        , CustomParser.map2
            (\() v ->
                CustomParser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            backSlash
            escapedCharValue
        , CustomParser.mapChompedString
            (\value () -> CustomParser.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


functionName : CustomParser.Parser String
functionName =
    CustomParser.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                charIsAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        , reserved = reservedList
        , start =
            \c -> Char.isLower c || Unicode.isLower c
        }


functionNameNotInfix : CustomParser.Parser String
functionNameNotInfix =
    CustomParser.variable
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


typeName : CustomParser.Parser String
typeName =
    CustomParser.variable
        { inner =
            \c ->
                -- checking for these common ranges early is much faster
                charIsAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
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


prefixOperatorToken : CustomParser.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> CustomParser.symbol token |> CustomParser.map (\() -> token))
        |> CustomParser.oneOf


minus : CustomParser.Parser ()
minus =
    CustomParser.symbol "-"


minusSymbols : CustomParser.Parser ()
minusSymbols =
    CustomParser.oneOf
        [ CustomParser.symbol "- "
        , CustomParser.symbol "-\n"
        , CustomParser.symbol "-\u{000D}"
        ]


dot : CustomParser.Parser ()
dot =
    CustomParser.symbol "."


dotDot : CustomParser.Parser ()
dotDot =
    CustomParser.symbol ".."


squareStart : CustomParser.Parser ()
squareStart =
    CustomParser.symbol "["


squareEnd : CustomParser.Parser ()
squareEnd =
    CustomParser.symbol "]"


curlyStart : CustomParser.Parser ()
curlyStart =
    CustomParser.symbol "{"


curlyEnd : CustomParser.Parser ()
curlyEnd =
    CustomParser.symbol "}"


pipe : CustomParser.Parser ()
pipe =
    CustomParser.symbol "|"


backSlash : CustomParser.Parser ()
backSlash =
    CustomParser.symbol "\\"


arrowRight : CustomParser.Parser ()
arrowRight =
    CustomParser.symbol "->"


equal : CustomParser.Parser ()
equal =
    CustomParser.symbol "="


comma : CustomParser.Parser ()
comma =
    CustomParser.symbol ","


parensStart : CustomParser.Parser ()
parensStart =
    CustomParser.symbol "("


parensEnd : CustomParser.Parser ()
parensEnd =
    CustomParser.symbol ")"


colon : CustomParser.Parser ()
colon =
    CustomParser.symbol ":"


cons : CustomParser.Parser ()
cons =
    CustomParser.symbol "::"
