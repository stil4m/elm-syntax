module Elm.Parser.Tokens exposing
    ( asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken, aliasToken
    , dot, dotDot, squareStart, squareEnd, curlyStart, curlyEnd, pipe, backSlash, arrowRight, equal, comma, parensStart, parensEnd, colon, cons
    , minus, minusSymbols
    , characterLiteral, stringLiteral, multiLineStringLiteral
    , functionName, typeName
    )

{-|

@docs asToken, caseToken, elseToken, exposingToken, ifToken, importToken, inToken, letToken, moduleToken, whereToken, ofToken, portToken, prefixOperatorToken, thenToken, aliasToken

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


aliasToken : Core.Parser ()
aliasToken =
    Core.symbol "alias"


escapedCharValue : Core.Parser Char
escapedCharValue =
    Core.oneOf
        [ Core.map (\() -> '\'') (Core.symbol "'")
        , Core.map (\() -> '"') (Core.symbol "\"")
        , Core.map (\() -> '\n') (Core.symbol "n")
        , Core.map (\() -> '\t') (Core.symbol "t")
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Core.map (\() -> '\u{000D}') (Core.symbol "r")
        , Core.map (\() -> '\\') (Core.symbol "\\")
        , Core.succeed
            (\hex ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            |. Core.symbol "u{"
            |= Core.variable
                { start = Char.isHexDigit
                , inner = Char.isHexDigit
                , reserved = Set.empty
                }
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
        , Core.succeed (\v -> Loop (stringSoFar ++ String.fromChar v ++ ""))
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
            |> Core.map (\() -> Loop (stringSoFar ++ "\""))
        , Core.succeed (\v -> Loop (stringSoFar ++ String.fromChar v ++ ""))
            |. Core.symbol "\\"
            |= escapedCharValue
        , Core.mapChompedString
            (\value () -> Loop (stringSoFar ++ value))
            (Core.chompWhile (\c -> c /= '"' && c /= '\\'))
        ]


functionName : Core.Parser String
functionName =
    Core.variable
        { start = \c -> Char.isLower c || Unicode.isLower c
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = reservedList
        }


typeName : Core.Parser String
typeName =
    Core.variable
        { start = \c -> Char.isUpper c || Unicode.isUpper c
        , inner =
            \c ->
                -- checking for Char.isAlphaNum early is much faster
                Char.isAlphaNum c || c == '_' || Unicode.isAlphaNum c
        , reserved = Set.empty
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
        |> List.map (\token -> Core.symbol token |> Core.map (\() -> token))
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
