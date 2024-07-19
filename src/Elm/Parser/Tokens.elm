module Elm.Parser.Tokens exposing
    ( asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken
    , dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
    , minus, minusSymbols
    , characterLiteral, stringLiteral, multiLineStringLiteral
    , functionName, typeName
    )

{-|

@docs asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken

@docs dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
@docs minus, minusSymbols

@docs characterLiteral, stringLiteral, multiLineStringLiteral
@docs functionName, typeName

-}

import Char
import Hex
import Parser as Core exposing ((|.), (|=), Step(..))
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


portToken : Core.Parser ()
portToken =
    Core.symbol "port"


moduleToken : Core.Parser ()
moduleToken =
    Core.symbol "module"


whereToken : Core.Parser ()
whereToken =
    Core.symbol "where"


exposingToken : Core.Parser ()
exposingToken =
    Core.symbol "exposing"


importToken : Core.Parser ()
importToken =
    Core.keyword "import"


asToken : Core.Parser ()
asToken =
    Core.keyword "as"


ifToken : Core.Parser ()
ifToken =
    Core.symbol "if"


thenToken : Core.Parser ()
thenToken =
    Core.symbol "then"


elseToken : Core.Parser ()
elseToken =
    Core.symbol "else"


caseToken : Core.Parser ()
caseToken =
    Core.symbol "case"


ofToken : Core.Parser ()
ofToken =
    Core.symbol "of"


letToken : Core.Parser ()
letToken =
    Core.symbol "let"


inToken : Core.Parser ()
inToken =
    Core.symbol "in"


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
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            |. Core.symbol "u"
            |. Core.symbol "{"
            |= (Core.chompWhile Char.isHexDigit |> Core.getChompedString)
            |. Core.symbol "}"
        ]


slashEscapedCharValue : Core.Parser Char
slashEscapedCharValue =
    Core.succeed identity
        |. Core.symbol "\\"
        |= escapedCharValue


characterLiteral : Core.Parser Char
characterLiteral =
    Core.succeed identity
        |. Core.symbol "'"
        |= Core.oneOf
            [ slashEscapedCharValue
            , Parser.Extra.anyChar
            ]
        |. Core.symbol "'"


stringLiteral : Core.Parser String
stringLiteral =
    Core.succeed identity
        |. Core.symbol "\""
        |= Core.loop "" stringLiteralHelper


stringLiteralHelper : String -> Core.Parser (Step String String)
stringLiteralHelper stringSoFar =
    Core.oneOf
        [ Core.symbol "\"" |> Core.map (\() -> Done stringSoFar)
        , Core.succeed (\v -> Loop (stringSoFar ++ String.fromChar v))
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


multiLineStringLiteral : Core.Parser String
multiLineStringLiteral =
    Core.succeed identity
        |. Core.symbol "\"\"\""
        |= Core.loop "" multiLineStringLiteralStep


multiLineStringLiteralStep : String -> Core.Parser (Step String String)
multiLineStringLiteralStep stringSoFar =
    Core.oneOf
        [ Core.symbol "\"\"\""
            |> Core.map (\() -> Done stringSoFar)
        , Core.symbol "\""
            |> Core.mapChompedString (\v () -> Loop (stringSoFar ++ v))
        , Core.succeed (\v -> Loop (stringSoFar ++ String.fromChar v))
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


functionName : Core.Parser String
functionName =
    Core.variable
        { start = Unicode.isLower
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = reservedList
        }


typeName : Core.Parser String
typeName =
    Core.variable
        { start = Unicode.isUpper
        , inner = \c -> Unicode.isAlphaNum c || c == '_'
        , reserved = reservedList
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


prefixOperatorToken : Core.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> Core.token token |> Core.getChompedString)
        |> Core.oneOf


minus : Core.Parser ()
minus =
    Core.symbol "-"


minusSymbols : Core.Parser ()
minusSymbols =
    Core.oneOf
        [ Core.symbol "- "
        , Core.symbol "-\n"
        , Core.symbol "-\u{000D}"
        ]


dot : Core.Parser ()
dot =
    Core.symbol "."


dotDot : Core.Parser ()
dotDot =
    Core.symbol ".."


squareStart : Core.Parser ()
squareStart =
    Core.symbol "["


squareEnd : Core.Parser ()
squareEnd =
    Core.symbol "]"


curlyStart : Core.Parser ()
curlyStart =
    Core.symbol "{"


curlyEnd : Core.Parser ()
curlyEnd =
    Core.symbol "}"


pipe : Core.Parser ()
pipe =
    Core.symbol "|"


backSlash : Core.Parser ()
backSlash =
    Core.symbol "\\"


arrowRight : Core.Parser ()
arrowRight =
    Core.symbol "->"


equal : Core.Parser ()
equal =
    Core.symbol "="


comma : Core.Parser ()
comma =
    Core.symbol ","


parensStart : Core.Parser ()
parensStart =
    Core.symbol "("


parensEnd : Core.Parser ()
parensEnd =
    Core.symbol ")"


colon : Core.Parser ()
colon =
    Core.symbol ":"


cons : Core.Parser ()
cons =
    Core.symbol "::"
