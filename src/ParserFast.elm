module ParserFast exposing
    ( Parser, run
    , int, number, symbol, symbolFollowedBy, keyword, keywordFollowedBy, variable, end
    , succeed, problem, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, ignore, andThen
    , orSucceed, orSucceedLazy, oneOf2, oneOf, backtrackable
    , nestableMultiComment
    , getChompedString, chompIf, chompIfFollowedBy, chompWhile, mapChompedString
    , withIndentSetToColumn, withIndent, columnIndentAndThen
    , mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition, columnAndThen, offsetSourceAndThen
    )

{-|

@docs Parser, run

@docs int, number, symbol, symbolFollowedBy, keyword, keywordFollowedBy, variable, variableWithoutReserved, anyChar, end


# Flow

@docs succeed, problem, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, ignore, andThen

@docs orSucceed, orSucceedLazy, oneOf2, oneOf, backtrackable


# Whitespace

@docs chompWhileWhitespaceFollowedBy, nestableMultiComment


# Chompers

@docs getChompedString, chompIf, chompAnyChar, chompIfFollowedBy, chompWhile, chompWhileMap, mapChompedString


# Indentation, Positions and source

@docs withIndentSetToColumn, withIndent, columnIndentAndThen
@docs mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition, columnAndThen, offsetSourceAndThen

-}

import Elm.Syntax.Range exposing (Location)
import Parser
import ParserFast.Advanced as A
import Set


{-| A `Parser` helps turn a `String` into nicely structured data. For example,
we can [`run`](#run) the [`int`](#int) parser to turn `String` to `Int`:

    run int "123456" == Ok 123456
    run int "3.1415" == Err ...

The cool thing is that you can combine `Parser` values to handle much more
complex scenarios.

-}
type alias Parser a =
    A.Parser Parser.Problem a


{-| Try a parser. Here are some examples using the [`keyword`](#keyword)
parser:

    run (keyword "true") "true" --> Ok ()

    run (keyword "true") "True" --> Err ..

    run (keyword "true") "false" --> Err ...

    run (keyword "true") "true!" --> Ok ()

Notice the last case! A `Parser` will chomp as much as possible and not worry
about the rest. Use the [`end`](#end) parser to ensure you made it to the end
of the string!

Currently reuses the `elm/parser` `DeadEnd` type to report problems
to avoid breaking changes

-}
run : Parser a -> String -> Result (List Parser.DeadEnd) a
run =
    A.run


{-| A parser that succeeds without chomping any characters.

    run (succeed 90210) "mississippi" == Ok 90210

    run (succeed 3.141) "mississippi" == Ok 3.141

    run (succeed ()) "mississippi" == Ok ()

    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions.

-}
succeed : a -> Parser a
succeed =
    A.succeed


{-| **Skip** values in a parser pipeline. For example, maybe we want to parse
some JavaScript variables:

    var : Parser String
    var =
        getChompedString <|
            succeed ()
                |> ParserFast.ignore chompIf isStartChar
                |> ParserFast.ignore chompWhile isInnerChar

    isStartChar : Char -> Bool
    isStartChar char =
        Char.isAlpha char || char == '_' || char == '$'

    isInnerChar : Char -> Bool
    isInnerChar char =
        isStartChar char || Char.isDigit char

`chompIf isStartChar` can chomp one character and produce a `()` value.
`chompWhile isInnerChar` can chomp zero or more characters and produce a `()`
value. The `ignore` operators are saying to still chomp all the characters, but
skip the two `()` values that get produced. No one cares about them.

-}
ignore : Parser ignore -> Parser keep -> Parser keep
ignore ignored kept =
    A.ignore kept ignored


{-| Helper to define recursive parsers. Say we want a parser for simple
boolean expressions:

    true

    false

    true || false

    true || (true || false)

Notice that a boolean expression might contain _other_ boolean expressions.
That means we will want to define our parser in terms of itself:

    type Boolean
        = MyTrue
        | MyFalse
        | MyOr Boolean Boolean

    boolean : Parser Boolean
    boolean =
        oneOf
            [ succeed MyTrue
                |> ParserFast.ignore keyword "true"
            , succeed MyFalse
                |> ParserFast.ignore keyword "false"
            , succeed MyOr
                |> ParserFast.ignore symbol "("
                |> ParserFast.ignore spaces
                |= lazy (\_ -> boolean)
                |> ParserFast.ignore spaces
                |> ParserFast.ignore symbol "||"
                |> ParserFast.ignore spaces
                |= lazy (\_ -> boolean)
                |> ParserFast.ignore spaces
                |> ParserFast.ignore symbol ")"
            ]

**Notice that `boolean` uses `boolean` in its definition!** In Elm, you can
only define a value in terms of itself it is behind a function call. So
`lazy` helps us define these self-referential parsers. (`andThen` can be used
for this as well!)

-}
lazy : (() -> Parser a) -> Parser a
lazy =
    A.lazy


{-| Parse one thing `andThen` parse another thing. This is useful when you want
to check on what you just parsed. For example, maybe you want U.S. zip codes
and `int` is not suitable because it does not allow leading zeros. You could
say:

    zipCode : Parser String
    zipCode =
        getChompedString (chompWhile Char.isDigit)
            |> andThen checkZipCode

    checkZipCode : String -> Parser String
    checkZipCode code =
        if String.length code == 5 then
            succeed code

        else
            problem "a U.S. zip code has exactly 5 digits"

First we chomp digits `andThen` we check if it is a valid U.S. zip code. We
`succeed` if it has exactly five digits and report a `problem` if not.

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example, this time using `andThen` to verify unicode code points.

**Note:** If you are using `andThen` recursively and blowing the stack, check
out the [`ParserFast.Advanced.loop`](ParserFast-Advanced#loop) function to limit stack usage.

-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
    A.andThen


columnAndThen : (Int -> Parser a) -> Parser a
columnAndThen =
    A.columnAndThen


{-| Can be used to verify the current indentation like this:

    checkIndent : Parser ()
    checkIndent =
        columnIndentAndThen (\indent column -> checkIndentHelp (indent <= column))

    checkIndentHelp : Bool -> Parser ()
    checkIndentHelp isIndented =
        if isIndented then
            succeed ()

        else
            problem "expecting more spaces"

So the `checkIndent` parser only succeeds when you are "deeper" than the
current indent level. You could use this to parse Elm-style `let` expressions.

-}
columnIndentAndThen : (Int -> Int -> Parser b) -> Parser b
columnIndentAndThen =
    A.columnIndentAndThen


{-| Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you chomp `"\n\n\n\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.

-}
offsetSourceAndThen : (Int -> String -> Parser a) -> Parser a
offsetSourceAndThen =
    A.offsetSourceAndThen


{-| Parse 2 parser in sequence and combine their results
-}
map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 =
    A.map2


map3 : (a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3 =
    A.map3


map4 : (a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser value
map4 =
    A.map4


map5 : (a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5 =
    A.map5


map6 : (a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6 =
    A.map6


map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser value
map7 =
    A.map7


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser value
map8 =
    A.map8


map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser value
map9 =
    A.map9


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the [`andThen`](#andThen) docs to see
an example usage.
-}
problem : String -> Parser a
problem msg =
    A.problem (Parser.Problem msg)


orSucceed : Parser a -> a -> Parser a
orSucceed =
    A.orSucceed


orSucceedLazy : Parser a -> (() -> a) -> Parser a
orSucceedLazy =
    A.orSucceedLazy


mapOrSucceed : (first -> choice) -> Parser first -> choice -> Parser choice
mapOrSucceed =
    A.mapOrSucceed


oneOf2Map :
    (first -> choice)
    -> Parser first
    -> (second -> choice)
    -> Parser second
    -> Parser choice
oneOf2Map =
    A.oneOf2Map


oneOf2 : Parser a -> Parser a -> Parser a
oneOf2 =
    A.oneOf2


{-| If you are parsing JSON, the values can be strings, floats, booleans,
arrays, objects, or null. You need a way to pick `oneOf` them! Here is a
sample of what that code might look like:

    type Json
        = Number Float
        | Boolean Bool
        | Null

    json : Parser Json
    json =
        oneOf
            [ map Number float
            , map (\_ -> Boolean True) (keyword "true")
            , map (\_ -> Boolean False) (keyword "false")
            , map (\_ -> Null) keyword "null"
            ]

This parser will keep trying parsers until `oneOf` them starts chomping
characters. Once a path is chosen, it does not come back and try the others.

**Note:** I highly recommend reading [this document][semantics] to learn how
`oneOf` and `backtrackable` interact. It is subtle and important!

[semantics]: https://github.com/elm/parser/blob/master/semantics.md

-}
oneOf : List (Parser a) -> Parser a
oneOf possibilities =
    A.oneOf (Parser.Problem "empty oneOf") possibilities


{-| Transform the result of a parser. Maybe you have a value that is
an integer or `null`:


    nullOrInt : Parser (Maybe Int)
    nullOrInt =
        oneOf
            [ map Just int
            , map (\_ -> Nothing) (keyword "null")
            ]

    -- run nullOrInt "0"    == Ok (Just 0)
    -- run nullOrInt "13"   == Ok (Just 13)
    -- run nullOrInt "null" == Ok Nothing
    -- run nullOrInt "zero" == Err ...

-}
map : (a -> b) -> Parser a -> Parser b
map =
    A.map


{-| It is quite tricky to use `backtrackable` well! It can be very useful, but
also can degrade performance and error message quality.

Read [this document](https://github.com/elm/parser/blob/master/semantics.md)
to learn how `oneOf`, `backtrackable`, and `commit` work and interact with
each other. It is subtle and important!

-}
backtrackable : Parser a -> Parser a
backtrackable =
    A.backtrackable



-- TOKEN


{-| Parse a bunch of different kinds of numbers without backtracking. A parser
for Elm would need to handle integers, floats, and hexadecimal like this:

    type Expr
        = Variable String
        | Int Int
        | Float Float
        | Apply Expr Expr

    elmNumber : Parser Expr
    elmNumber =
        number
            { int = Just Int
            , hex = Just Int -- 0x001A is allowed
            , octal = Nothing -- 0o0731 is not
            , binary = Nothing -- 0b1101 is not
            , float = Just Float
            }

If you wanted to implement a float parser, it would be like this:

    float : Parser Float
    float =
        number
            { int = Just toFloat
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just identity
            }

Notice that it actually is processing `int` results! This is because `123`
looks like an integer to me, but maybe it looks like a float to you. If you had
`int = Nothing`, floats would need a decimal like `1.0` in every case. If you
like explicitness, that may actually be preferable!

**Note:** This function does not check for weird trailing characters in the
current implementation, so parsing `123abc` can succeed up to `123` and then
move on. This is helpful for people who want to parse things like `40px` or
`3m`, but it requires a bit of extra code to rule out trailing characters in
other cases.

-}
number :
    { binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    , hex : Maybe (Int -> a)
    , int : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    }
    -> Parser a
number i =
    A.number
        { binary = Result.fromMaybe Parser.ExpectingBinary i.binary
        , expecting = Parser.ExpectingNumber
        , float = Result.fromMaybe Parser.ExpectingFloat i.float
        , hex = Result.fromMaybe Parser.ExpectingHex i.hex
        , int = Result.fromMaybe Parser.ExpectingInt i.int
        , invalid = Parser.ExpectingNumber
        , octal = Result.fromMaybe Parser.ExpectingOctal i.octal
        }


{-| Parse integers.

    run int "1"    == Ok 1
    run int "1234" == Ok 1234

    run int "-789" == Err ...
    run int "0123" == Err ...
    run int "1.34" == Err ...
    run int "1e31" == Err ...
    run int "123a" == Err ...
    run int "0x1A" == Err ...

If you want to handle a leading `+` or `-` you should do it with a custom
parser like this:

    myInt : Parser Int
    myInt =
        oneOf
            [ succeed negate
                |> ParserFast.ignore symbol "-"
                |= int
            , int
            ]

**Note:** If you want a parser for both `Int` and `Float` literals, check out
[`number`](#number) below. It will be faster than using `oneOf` to combining
`int` and `float` yourself.

-}
int : Parser Int
int =
    A.number
        { binary = Err Parser.ExpectingInt
        , expecting = Parser.ExpectingInt
        , float = Err Parser.ExpectingInt
        , hex = Err Parser.ExpectingInt
        , int = Ok identity
        , invalid = Parser.ExpectingInt
        , octal = Err Parser.ExpectingInt
        }


{-| Parse symbols like `(` and `,`.
Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.

    run (symbol "[") "[" == Ok ()
    run (symbol "[") "4" == Err ... (ExpectingSymbol "[") ...

**Note:** This is good for stuff like brackets and semicolons, but it probably
should not be used for binary operators like `+` and `-` because you can find
yourself in weird situations. For example, is `3--4` a typo? Or is it `3 - -4`?
I have had better luck with `chompWhile isSymbol` and sorting out which
operator it is afterwards.

-}
symbol : String -> res -> Parser res
symbol str res =
    A.symbol str (Parser.ExpectingSymbol str) res


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
symbolFollowedBy : String -> Parser next -> Parser next
symbolFollowedBy str nextParser =
    A.symbolFollowedBy str (Parser.ExpectingSymbol str) nextParser


{-| Parse keywords like `let`, `case`, and `type`.
Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.

    run (keyword "let") "let"     == Ok ()
    run (keyword "let") "var"     == Err ... (ExpectingKeyword "let") ...
    run (keyword "let") "letters" == Err ... (ExpectingKeyword "let") ...

**Note:** Notice the third case there! `keyword` actually looks ahead one
character to make sure it is not a letter, number, or underscore. The goal is
to help with parsers like this:

    succeed identity
        |> ParserFast.ignore keyword "let"
        |> ParserFast.ignore spaces
        |= elmVar
        |> ParserFast.ignore spaces
        |> ParserFast.ignore symbol "="

The trouble is that `spaces` may chomp zero characters (to handle expressions
like `[1,2]` and `[ 1 , 2 ]`) and in this case, it would mean `letters` could
be parsed as `let ters` and then wonder where the equals sign is! Check out the
[`symbol`](#symbol) docs if you need to customize this!

-}
keyword : String -> res -> Parser res
keyword kwd res =
    A.keyword kwd (Parser.ExpectingKeyword kwd) res


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
keywordFollowedBy : String -> Parser next -> Parser next
keywordFollowedBy kwd nextParser =
    A.keywordFollowedBy kwd (Parser.ExpectingKeyword kwd) nextParser


{-| Check if you have reached the end of the string you are parsing.


    justAnInt : Parser Int
    justAnInt =
        succeed identity
            |= int
            |> ParserFast.ignore end

    -- run justAnInt "90210" == Ok 90210
    -- run justAnInt "1 + 2" == Err ...
    -- run int       "1 + 2" == Ok 1

Parsers can succeed without parsing the whole string. Ending your parser
with `end` guarantees that you have successfully parsed the whole string.

-}
end : Parser ()
end =
    A.end Parser.ExpectingEnd


{-| Sometimes parsers like `int` or `variable` cannot do exactly what you
need. The "chomping" family of functions is meant for that case! Maybe you
need to parse [valid PHP variables][php] like `$x` and `$txt`:

    php : Parser String
    php =
        getChompedString <|
            succeed ()
                |> ParserFast.ignore chompIf (\c -> c == '$')
                |> ParserFast.ignore chompIf (\c -> Char.isAlpha c || c == '_')
                |> ParserFast.ignore chompWhile (\c -> Char.isAlphaNum c || c == '_')

The idea is that you create a bunch of chompers that validate the underlying
characters. Then `getChompedString` extracts the underlying `String` efficiently.

[php]: https://www.w3schools.com/php/php_variables.asp

-}
getChompedString : Parser a -> Parser String
getChompedString =
    A.getChompedString


{-| This works just like [`getChompedString`](#getChompedString) but gives
a bit more flexibility. For example, maybe you want to parse Elm doc comments
and get (1) the full comment and (2) all of the names listed in the docs.

You could implement `mapChompedString` like this:

    mapChompedString : (String -> a -> b) -> Parser a -> Parser String
    mapChompedString func parser =
        succeed (\start value end src -> func (String.slice start end src) value)
            |= getOffset
            |= parser
            |= getOffset
            |= getSource

-}
mapChompedString : (String -> a -> b) -> Parser a -> Parser b
mapChompedString =
    A.mapChompedString


{-| Chomp one character if it passes the test.

    chompUpper : Parser ()
    chompUpper =
        chompIf Char.isUpper

So this can chomp a character like `T` and produces a `()` value.

-}
chompIf : (Char -> Bool) -> Parser ()
chompIf isGood =
    A.chompIf isGood Parser.UnexpectedChar


chompAnyChar : Parser ()
chompAnyChar =
    A.chompAnyChar Parser.UnexpectedChar


anyChar : Parser Char
anyChar =
    A.anyChar Parser.UnexpectedChar


chompIfFollowedBy : (Char -> Bool) -> Parser a -> Parser a
chompIfFollowedBy isGood nextParser =
    A.chompIfFollowedBy isGood Parser.UnexpectedChar nextParser


{-| Chomp zero or more characters if they pass the test. This is commonly
useful for chomping whitespace or variable names:

    whitespace : Parser ()
    whitespace =
        chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')

    elmVar : Parser String
    elmVar =
        getChompedString <|
            succeed ()
                |> ParserFast.ignore chompIf Char.isLower
                |> ParserFast.ignore chompWhile (\c -> Char.isAlphaNum c || c == '_')

**Note:** a `chompWhile` parser always succeeds! This can lead to tricky
situations, especially if you define your whitespace with it. In that case,
you could accidentally interpret `letx` as the keyword `let` followed by
"spaces" followed by the variable `x`. This is why the `keyword` and `number`
parsers peek ahead, making sure they are not followed by anything unexpected.

-}
chompWhile : (Char -> Bool) -> Parser ()
chompWhile =
    A.chompWhile


chompWhileMap : (Char -> Bool) -> (String -> res) -> Parser res
chompWhileMap =
    A.chompWhileMap


{-| Specialized `chompWhile (\c -> c == " " || c == "\n" || c == "\r")`
optimized for speed
-}
chompWhileWhitespaceFollowedBy : Parser next -> Parser next
chompWhileWhitespaceFollowedBy =
    A.chompWhileWhitespaceFollowedBy


{-| Some languages are indentation sensitive. Python cares about tabs. Elm
cares about spaces sometimes. `withIndent` and `getIndent` allow you to manage
"indentation state" yourself, however is necessary in your scenario.

@test-helper

-}
withIndent : Int -> Parser a -> Parser a
withIndent =
    A.withIndent


{-| For a given ParserWithComments.Parser, take the current start column as indentation for the whole block
-}
withIndentSetToColumn : Parser a -> Parser a
withIndentSetToColumn =
    A.withIndentSetToColumn


mapWithStartPosition :
    (Location -> a -> b)
    -> Parser a
    -> Parser b
mapWithStartPosition =
    A.mapWithStartPosition


mapWithEndPosition :
    (a -> Location -> b)
    -> Parser a
    -> Parser b
mapWithEndPosition =
    A.mapWithEndPosition


mapWithStartAndEndPosition :
    (Location -> a -> Location -> b)
    -> Parser a
    -> Parser b
mapWithStartAndEndPosition =
    A.mapWithStartAndEndPosition


{-| Create a parser for variables. If we wanted to parse type variables in Elm,
we could try something like this:

    import Char
    import ParserFast exposing (..)
    import Set

    typeVar : Parser String
    typeVar =
        variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "let", "in", "case", "of" ]
            }

This is saying it _must_ start with a lower-case character. After that,
characters can be letters, numbers, or underscores. It is also saying that if
you run into any of these reserved names, it is definitely not a variable.

-}
variable :
    { inner : Char -> Bool
    , reserved : Set.Set String
    , start : Char -> Bool
    }
    -> Parser String
variable i =
    A.variable
        { expecting = Parser.ExpectingVariable
        , inner = i.inner
        , reserved = i.reserved
        , start = i.start
        }


variableWithoutReserved :
    { start : Char -> Bool
    , inner : Char -> Bool
    }
    -> Parser String
variableWithoutReserved i =
    A.variableWithoutReserved i
        Parser.ExpectingVariable


{-| Parse multi-line comments. So if you wanted to parse Elm whitespace or
JS whitespace, you could say:

    elm : Parser ()
    elm =
        loop 0 <|
            ifProgress <|
                oneOf
                    [ lineComment "--"
                    , multiComment "{-" "-}" Nestable
                    , spaces
                    ]

    js : Parser ()
    js =
        loop 0 <|
            ifProgress <|
                oneOf
                    [ lineComment "//"
                    , multiComment "/*" "*/" NotNestable
                    , chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
                    ]

    ifProgress : Parser a -> Int -> Parser (Step Int ())
    ifProgress parser offset =
        succeed identity
            |> ParserFast.ignore parser
            |= getOffset
            |> map
                (\newOffset ->
                    if offset == newOffset then
                        Done ()

                    else
                        Loop newOffset
                )

**Note:** The fact that `spaces` comes last in the definition of `elm` is very
important! It can succeed without consuming any characters, so if it were the
first option, it would always succeed and bypass the others! (Same is true of
`chompWhile` in `js`.) This possibility of success without consumption is also
why wee need the `ifProgress` helper. It detects if there is no more whitespace
to consume.

-}
nestableMultiComment : String -> String -> Parser ()
nestableMultiComment open close =
    A.nestableMultiComment open (Parser.Expecting open) close (Parser.Expecting close)
