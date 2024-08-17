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
import Hex
import ParserFast
import ParserFast.Advanced
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
                (ParserFast.ifFollowedByWhile
                    Char.isHexDigit
                    Char.isHexDigit
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
                ParserFast.anyChar
            )
        )
        (ParserFast.symbol "'" ())


singleOrTripleQuotedStringLiteral : ParserFast.Parser String
singleOrTripleQuotedStringLiteral =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2
            (ParserFast.symbolFollowedBy "\"\""
                (ParserFast.Advanced.loop ""
                    tripleQuotedStringLiteralStep
                    (\maybeExtension soFar ->
                        case maybeExtension of
                            Nothing ->
                                ParserFast.Advanced.Done soFar

                            Just extension ->
                                ParserFast.Advanced.Loop (soFar ++ extension ++ "")
                    )
                )
            )
            (ParserFast.Advanced.loop ""
                stringLiteralStep
                (\maybeExtension soFar ->
                    case maybeExtension of
                        Nothing ->
                            ParserFast.Advanced.Done soFar

                        Just extension ->
                            ParserFast.Advanced.Loop (soFar ++ extension ++ "")
                )
            )
        )


stringLiteralStep : ParserFast.Parser (Maybe String)
stringLiteralStep =
    ParserFast.oneOf3
        (ParserFast.symbol "\"" Nothing)
        (ParserFast.map
            (\v -> Just (String.fromChar v))
            (ParserFast.symbolFollowedBy "\\" escapedCharValue)
        )
        (ParserFast.whileMap (\c -> c /= '"' && c /= '\\') Just)


tripleQuotedStringLiteralStep : ParserFast.Parser (Maybe String)
tripleQuotedStringLiteralStep =
    ParserFast.oneOf
        [ ParserFast.symbol "\"\"\"" Nothing
        , ParserFast.symbol "\"" (Just "\"")
        , ParserFast.map (\v -> Just (String.fromChar v))
            (ParserFast.symbolFollowedBy "\\" escapedCharValue)
        , ParserFast.whileMap (\c -> c /= '"' && c /= '\\') Just
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
    ParserFast.ifFollowedByWhile
        (\c -> Char.isUpper c || Unicode.isUpper c)
        (\c ->
            -- checking for these common ranges early is much faster
            Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
        )


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
