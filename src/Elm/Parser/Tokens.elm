module Elm.Parser.Tokens exposing
    ( inToken
    , squareEnd, curlyEnd, arrowRight, equal, parensEnd
    , minusFollowedBySingleWhitespace
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNotInfix, typeName
    )

{-|

@docs inToken

@docs squareEnd, curlyEnd, arrowRight, equal, parensEnd
@docs minusFollowedBySingleWhitespace
@docs prefixOperatorToken, allowedOperatorTokens

@docs characterLiteral, singleOrTripleQuotedStringLiteral
@docs functionName, functionNameNotInfix, typeName

-}

import Char
import Char.Extra
import Elm.Syntax.StringLiteralType exposing (StringLiteralType(..))
import Hex
import ParserFast
import ParserFast.Advanced
import ParserFast.Extra
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


inToken : ParserFast.Parser ()
inToken =
    ParserFast.keyword "in" ()


escapedCharValue : ParserFast.Parser Char
escapedCharValue =
    ParserFast.oneOf
        [ ParserFast.symbol "'" '\''
        , ParserFast.symbol "\"" '"'
        , ParserFast.symbol "n" '\n'
        , ParserFast.symbol "t" '\t'
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          ParserFast.symbol "r" '\u{000D}'
        , ParserFast.symbol "\\" '\\'
        , ParserFast.map2
            (\hex () ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            (ParserFast.symbolFollowedBy "u{"
                (ParserFast.variable
                    { inner = Char.isHexDigit
                    , reserved = Set.empty
                    , start = Char.isHexDigit
                    }
                )
            )
            (ParserFast.symbol "}" ())
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


singleOrTripleQuotedStringLiteral : ParserFast.Parser ( StringLiteralType, String )
singleOrTripleQuotedStringLiteral =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy "\"\""
                (ParserFast.Advanced.loop "" tripleQuotedStringLiteralStep)
                |> ParserFast.map (\str -> ( TripleQuote, str ))
            )
            (ParserFast.Advanced.loop "" stringLiteralHelper
                |> ParserFast.map (\str -> ( SingleQuote, str ))
            )
        )


stringLiteralHelper : String -> ParserFast.Parser (ParserFast.Advanced.Step String String)
stringLiteralHelper stringSoFar =
    ParserFast.oneOf
        [ ParserFast.symbol "\"" (ParserFast.Advanced.Done stringSoFar)
        , ParserFast.map
            (\v ->
                ParserFast.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            (ParserFast.symbolFollowedBy "\\" escapedCharValue)
        , ParserFast.mapChompedString
            (\value () -> ParserFast.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


chompWhileIsInsideString : ParserFast.Parser ()
chompWhileIsInsideString =
    ParserFast.chompWhile (\c -> c /= '"' && c /= '\\')


tripleQuotedStringLiteralStep : String -> ParserFast.Parser (ParserFast.Advanced.Step String String)
tripleQuotedStringLiteralStep stringSoFar =
    ParserFast.oneOf
        [ ParserFast.symbol "\"\"\"" (ParserFast.Advanced.Done stringSoFar)
        , ParserFast.symbol "\"" (ParserFast.Advanced.Loop (stringSoFar ++ "\""))
        , ParserFast.map
            (\v ->
                ParserFast.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            (ParserFast.symbolFollowedBy "\\" escapedCharValue)
        , ParserFast.mapChompedString
            (\value () -> ParserFast.Advanced.Loop (stringSoFar ++ value ++ ""))
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


functionNameNotInfix : ParserFast.Parser String
functionNameNotInfix =
    ParserFast.variable
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
