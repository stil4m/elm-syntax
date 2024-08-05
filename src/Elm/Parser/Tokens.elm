module Elm.Parser.Tokens exposing
    ( inToken
    , squareEnd, curlyEnd, arrowRight, equal, parensEnd
    , minus, minusSymbols
    , prefixOperatorToken, allowedOperatorTokens
    , characterLiteral, singleOrTripleQuotedStringLiteral
    , functionName, functionNameNotInfix, typeName
    )

{-|

@docs inToken

@docs squareEnd, curlyEnd, arrowRight, equal, parensEnd
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


inToken : CustomParser.Parser ()
inToken =
    CustomParser.keyword "in" ()


escapedCharValue : CustomParser.Parser Char
escapedCharValue =
    CustomParser.oneOf
        [ CustomParser.symbol "'" '\''
        , CustomParser.symbol "\"" '"'
        , CustomParser.symbol "n" '\n'
        , CustomParser.symbol "t" '\t'
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          CustomParser.symbol "r" '\u{000D}'
        , CustomParser.symbol "\\" '\\'
        , CustomParser.map2
            (\hex () ->
                case String.toLower hex |> Hex.fromString of
                    Ok n ->
                        Char.fromCode n

                    Err _ ->
                        '\u{0000}'
            )
            (CustomParser.symbolFollowedBy "u{"
                (CustomParser.variable
                    { inner = Char.isHexDigit
                    , reserved = Set.empty
                    , start = Char.isHexDigit
                    }
                )
            )
            (CustomParser.symbol "}" ())
        ]


slashEscapedCharValue : CustomParser.Parser Char
slashEscapedCharValue =
    CustomParser.symbolFollowedBy "\\" escapedCharValue


characterLiteral : CustomParser.Parser Char
characterLiteral =
    CustomParser.map2
        (\res () -> res)
        (CustomParser.symbolFollowedBy "'"
            (CustomParser.oneOf
                [ slashEscapedCharValue
                , CustomParser.Extra.anyChar
                ]
            )
        )
        (CustomParser.symbol "'" ())


singleOrTripleQuotedStringLiteral : CustomParser.Parser String
singleOrTripleQuotedStringLiteral =
    CustomParser.symbolFollowedBy "\""
        (CustomParser.oneOf
            [ CustomParser.symbolFollowedBy "\"\""
                (CustomParser.Advanced.loop "" tripleQuotedStringLiteralStep)
            , CustomParser.Advanced.loop "" stringLiteralHelper
            ]
        )


stringLiteralHelper : String -> CustomParser.Parser (CustomParser.Advanced.Step String String)
stringLiteralHelper stringSoFar =
    CustomParser.oneOf
        [ CustomParser.symbol "\"" (CustomParser.Advanced.Done stringSoFar)
        , CustomParser.map
            (\v ->
                CustomParser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            (CustomParser.symbolFollowedBy "\\" escapedCharValue)
        , CustomParser.mapChompedString
            (\value () -> CustomParser.Advanced.Loop (stringSoFar ++ value ++ ""))
            chompWhileIsInsideString
        ]


chompWhileIsInsideString : CustomParser.Parser ()
chompWhileIsInsideString =
    CustomParser.chompWhile (\c -> c /= '"' && c /= '\\')


tripleQuotedStringLiteralStep : String -> CustomParser.Parser (CustomParser.Advanced.Step String String)
tripleQuotedStringLiteralStep stringSoFar =
    CustomParser.oneOf
        [ CustomParser.symbol "\"\"\"" (CustomParser.Advanced.Done stringSoFar)
        , CustomParser.symbol "\"" (CustomParser.Advanced.Loop (stringSoFar ++ "\""))
        , CustomParser.map
            (\v ->
                CustomParser.Advanced.Loop (stringSoFar ++ String.fromChar v ++ "")
            )
            (CustomParser.symbolFollowedBy "\\" escapedCharValue)
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
                Char.Extra.isAlphaNumFast c || c == '_' || Unicode.isAlphaNum c
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


prefixOperatorToken : CustomParser.Parser String
prefixOperatorToken =
    allowedOperatorTokens
        |> List.map (\token -> CustomParser.symbol token token)
        |> CustomParser.oneOf


minus : CustomParser.Parser ()
minus =
    CustomParser.symbol "-" ()


minusSymbols : CustomParser.Parser ()
minusSymbols =
    CustomParser.oneOf
        [ CustomParser.symbol "- " ()
        , CustomParser.symbol "-\n" ()
        , CustomParser.symbol "-\u{000D}" ()
        ]


squareEnd : CustomParser.Parser ()
squareEnd =
    CustomParser.symbol "]" ()


curlyEnd : CustomParser.Parser ()
curlyEnd =
    CustomParser.symbol "}" ()


arrowRight : CustomParser.Parser ()
arrowRight =
    CustomParser.symbol "->" ()


equal : CustomParser.Parser ()
equal =
    CustomParser.symbol "=" ()


parensEnd : CustomParser.Parser ()
parensEnd =
    CustomParser.symbol ")" ()
