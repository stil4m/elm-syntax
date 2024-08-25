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

@docs int, intOrHex, floatOrIntOrHex, symbol, symbolBacktrackable, symbolWithEndPosition, symbolWithStartAndEndPosition, symbolFollowedBy, followedBySymbol, keyword, keywordFollowedBy, while, whileWithoutLinebreak, whileMap, ifFollowedByWhileWithoutLinebreak, ifFollowedByWhileMapWithStartAndEndPositionWithoutLinebreak, ifFollowedByWhileExceptWithoutLinebreak, ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak, anyChar, end


# Flow

@docs succeed, problem, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, ignore, andThen

@docs orSucceed, orSucceedLazy, oneOf2, oneOf, backtrackable

@docs loopWhileSucceeds, loopUntil


# Whitespace

@docs chompWhileWhitespaceFollowedBy, nestableMultiComment


# Indentation, Positions and source

@docs withIndentSetToColumn, withIndent, columnIndentAndThen, validateEndColumnIndentation, validateEndColumnIndentationBacktrackable
@docs mapWithStartAndEndPosition, columnAndThen, offsetSourceAndThen

-}

import Elm.Syntax.Range exposing (Location)
import Parser
import Parser.Advanced exposing ((|=))
import Set


{-| A `Parser` helps turn a `String` into nicely structured data. For example,
we can [`run`](#run) the [`int`](#int) parser to turn `String` to `Int`:

    run int "123456" == Ok 123456
    run int "3.1415" == Err ...

The cool thing is that you can combine `Parser` values to handle much more
complex scenarios.

-}
type Parser a
    = Parser (State -> PStep Parser.Problem a)


type PStep problem value
    = Good Bool value State
    | Bad Bool (RopeFilled (DeadEnd problem)) ()


type alias State =
    { src : String
    , offset : Int
    , indent : Int
    , row : Int
    , col : Int
    }


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
run (Parser parse) src =
    case parse { src = src, offset = 0, indent = 1, row = 1, col = 1 } of
        Good _ value _ ->
            Ok value

        Bad _ deadEnds () ->
            Err (ropeFilledToList deadEnds [])


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

    { row = 18
    , col = 22
    , problem = UnexpectedComma
    }

We have a ton of information here! So in the error message, we can say that “I
ran into an issue when parsing a list in the definition of `viewHealthData`. It
looks like there is an extra comma.” Or maybe something even better!

Furthermore, many parsers just put a mark where the problem manifested. By
tracking the `row` and `col` of the context, we can show a much larger region
as a way of indicating “I thought I was parsing this thing that starts over
here.” Otherwise you can get very confusing error messages on a missing `]` or
`}` or `)` because “I need more indentation” on something unrelated.

**Note:** Rows and columns are counted like a text editor. The beginning is `row=1`
and `col=1`. The `col` increments as characters are chomped. When a `\n` is chomped,
`row` is incremented and `col` starts over again at `1`.

-}
type alias DeadEnd problem =
    { row : Int
    , col : Int
    , problem : problem
    }


type RopeFilled a
    = One a ()
    | Append (RopeFilled a) (RopeFilled a)


fromState : State -> x -> RopeFilled (DeadEnd x)
fromState s x =
    One { row = s.row, col = s.col, problem = x } ()


ropeFilledToList : RopeFilled x -> List x -> List x
ropeFilledToList ropeFilled list =
    case ropeFilled of
        One x () ->
            x :: list

        Append ropefilled1 ropefilled2 ->
            ropeFilledToList ropefilled1 (ropeFilledToList ropefilled2 list)


{-| A parser that succeeds without chomping any characters.

    run (succeed 90210) "mississippi" == Ok 90210

    run (succeed 3.141) "mississippi" == Ok 3.141

    run (succeed ()) "mississippi" == Ok ()

    run (succeed Nothing) "mississippi" == Ok Nothing

Seems weird on its own, but it is very useful in combination with other
functions.

-}
succeed : a -> Parser a
succeed a =
    Parser (\s -> Good False a s)


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
lazy thunk =
    Parser
        (\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )


validate : (a -> Bool) -> String -> Parser a -> Parser a
validate isOkay problemOnNotOkay (Parser parseA) =
    let
        xOnNotOkay : Parser.Problem
        xOnNotOkay =
            Parser.Problem problemOnNotOkay
    in
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                (Good committed a s1) as good ->
                    if isOkay a then
                        good

                    else
                        Bad committed (fromState s1 xOnNotOkay) ()
        )


columnAndThen : (Int -> Parser a) -> Parser a
columnAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col
            in
            parse s
        )


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
columnIndentAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col s.indent
            in
            parse s
        )


validateEndColumnIndentation : (Int -> Int -> Bool) -> String -> Parser a -> Parser a
validateEndColumnIndentation isOkay problemOnIsNotOkay (Parser parse) =
    let
        xOnIsNotOkay : Parser.Problem
        xOnIsNotOkay =
            Parser.Problem problemOnIsNotOkay
    in
    Parser
        (\s0 ->
            case parse s0 of
                (Good committed _ s1) as good ->
                    if isOkay s1.col s1.indent then
                        good

                    else
                        Bad committed (fromState s1 xOnIsNotOkay) ()

                bad ->
                    bad
        )


validateEndColumnIndentationBacktrackable : (Int -> Int -> Bool) -> String -> Parser a -> Parser a
validateEndColumnIndentationBacktrackable isOkay problemOnIsNotOkay (Parser parse) =
    let
        xOnIsNotOkay : Parser.Problem
        xOnIsNotOkay =
            Parser.Problem problemOnIsNotOkay
    in
    Parser
        (\s0 ->
            case parse s0 of
                Good _ res s1 ->
                    if isOkay s1.col s1.indent then
                        Good False res s1

                    else
                        Bad False (fromState s1 xOnIsNotOkay) ()

                Bad _ x () ->
                    Bad False x ()
        )


{-| Editors think of code as a grid, but behind the scenes it is just a flat
array of UTF-16 characters. `getOffset` tells you your index in that flat
array. So if you chomp `"\n\n\n\n"` you are on row 5, column 1, and offset 4.

**Note:** JavaScript uses a somewhat odd version of UTF-16 strings, so a single
character may take two slots. So in JavaScript, `'abc'.length === 3` but
`'🙈🙉🙊'.length === 6`. Try it out! And since Elm runs in JavaScript, the offset
moves by those rules.

-}
offsetSourceAndThen : (Int -> String -> Parser a) -> Parser a
offsetSourceAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.offset s.src
            in
            parse s
        )


{-| Parse 2 parser in sequence and combine their results
-}
map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            Good (c1 || c2) (func a b) s2
        )


map2WithStartPosition : (Location -> a -> b -> value) -> Parser a -> Parser b -> Parser value
map2WithStartPosition func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            Good (c1 || c2) (func { row = s0.row, column = s0.col } a b) s2
        )


map2WithStartAndEndPosition : (Location -> a -> b -> Location -> value) -> Parser a -> Parser b -> Parser value
map2WithStartAndEndPosition func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            Good (c1 || c2) (func { row = s0.row, column = s0.col } a b { row = s2.row, column = s2.col }) s2
        )


map3 : (a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3 func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    Good (c1 || c2 || c3) (func a b c) s3
        )


map3WithStartAndEndPosition : (Location -> a -> b -> c -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser value
map3WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    Good (c1 || c2 || c3) (func { row = s0.row, column = s0.col } a b c { row = s3.row, column = s3.col }) s3
        )


map4 : (a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser value
map4 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            Good (c1 || c2 || c3 || c4) (func a b c d) s4
        )


map4WithStartAndEndPosition : (Location -> a -> b -> c -> d -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser value
map4WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            Good (c1 || c2 || c3 || c4) (func { row = s0.row, column = s0.col } a b c d { row = s4.row, column = s4.col }) s4
        )


map5 : (a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    Good (c1 || c2 || c3 || c4 || c5) (func a b c d e) s5
        )


map5WithStartPosition : (Location -> a -> b -> c -> d -> e -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5WithStartPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    Good (c1 || c2 || c3 || c4 || c5) (func { row = s0.row, column = s0.col } a b c d e) s5
        )


map5WithStartAndEndPosition : (Location -> a -> b -> c -> d -> e -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser value
map5WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    Good (c1 || c2 || c3 || c4 || c5) (func { row = s0.row, column = s0.col } a b c d e { row = s5.row, column = s5.col }) s5
        )


map6 : (a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            Good (c1 || c2 || c3 || c4 || c5 || c6) (func a b c d e f) s6
        )


map6WithStartPosition : (Location -> a -> b -> c -> d -> e -> f -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6WithStartPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            Good (c1 || c2 || c3 || c4 || c5 || c6) (func { row = s0.row, column = s0.col } a b c d e f) s6
        )


map6WithStartAndEndPosition : (Location -> a -> b -> c -> d -> e -> f -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser value
map6WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            Good (c1 || c2 || c3 || c4 || c5 || c6) (func { row = s0.row, column = s0.col } a b c d e f { row = s6.row, column = s6.col }) s6
        )


map7WithStartAndEndPosition : (Location -> a -> b -> c -> d -> e -> f -> g -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser value
map7WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    Good (c1 || c2 || c3 || c4 || c5 || c6 || c7) (func { row = s0.row, column = s0.col } a b c d e f g { row = s7.row, column = s7.col }) s7
        )


map8WithStartPosition : (Location -> a -> b -> c -> d -> e -> f -> g -> h -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser value
map8WithStartPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad c8 x () ->
                                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) x ()

                                                                        Good c8 h s8 ->
                                                                            Good (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) (func { row = s0.row, column = s0.col } a b c d e f g h) s8
        )


map9WithStartAndEndPosition : (Location -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Location -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser value
map9WithStartAndEndPosition func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad committed x () ->
                    Bad committed x ()

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            Bad (c1 || c2) x ()

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    Bad (c1 || c2 || c3) x ()

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            Bad (c1 || c2 || c3 || c4) x ()

                                        Good c4 d s4 ->
                                            case parseE s4 of
                                                Bad c5 x () ->
                                                    Bad (c1 || c2 || c3 || c4 || c5) x ()

                                                Good c5 e s5 ->
                                                    case parseF s5 of
                                                        Bad c6 x () ->
                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6) x ()

                                                        Good c6 f s6 ->
                                                            case parseG s6 of
                                                                Bad c7 x () ->
                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7) x ()

                                                                Good c7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad c8 x () ->
                                                                            Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8) x ()

                                                                        Good c8 h s8 ->
                                                                            case parseI s8 of
                                                                                Bad c9 x () ->
                                                                                    Bad (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8 || c9) x ()

                                                                                Good c9 i s9 ->
                                                                                    Good (c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8 || c9) (func { row = s0.row, column = s0.col } a b c d e f g h i { row = s9.row, column = s9.col }) s9
        )


{-| Indicate that a parser has reached a dead end. "Everything was going fine
until I ran into this problem." Check out the -AndThen helpers for where to use this.
-}
problem : String -> Parser a
problem msg =
    let
        x : Parser.Problem
        x =
            Parser.Problem msg
    in
    Parser (\s -> Bad False (fromState s x) ())


orSucceed : Parser a -> a -> Parser a
orSucceed (Parser attemptFirst) secondRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _ ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        Good False secondRes s
        )


mapOrSucceed : (a -> b) -> Parser a -> b -> Parser b
mapOrSucceed valueChange (Parser parse) fallback =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed value s1 ->
                    Good committed (valueChange value) s1

                Bad firstCommitted x () ->
                    if firstCommitted then
                        Bad True x ()

                    else
                        Good False fallback s0
        )


map2OrSucceed : (a -> b -> value) -> Parser a -> Parser b -> value -> Parser value
map2OrSucceed func (Parser parseA) (Parser parseB) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x () ->
                    if c1 then
                        Bad True x ()

                    else
                        Good False fallback s0

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            if c1 || c2 then
                                Bad True x ()

                            else
                                Good False fallback s0

                        Good c2 b s2 ->
                            Good (c1 || c2) (func a b) s2
        )


map3OrSucceed : (a -> b -> c -> value) -> Parser a -> Parser b -> Parser c -> value -> Parser value
map3OrSucceed func (Parser parseA) (Parser parseB) (Parser parseC) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x () ->
                    if c1 then
                        Bad True x ()

                    else
                        Good False fallback s0

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            if c1 || c2 then
                                Bad True x ()

                            else
                                Good False fallback s0

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    if c1 || c2 || c3 then
                                        Bad True x ()

                                    else
                                        Good False fallback s0

                                Good c3 c s3 ->
                                    Good (c1 || c2 || c3) (func a b c) s3
        )


map4OrSucceed : (a -> b -> c -> d -> value) -> Parser a -> Parser b -> Parser c -> Parser d -> value -> Parser value
map4OrSucceed func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) fallback =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad c1 x () ->
                    if c1 then
                        Bad True x ()

                    else
                        Good False fallback s0

                Good c1 a s1 ->
                    case parseB s1 of
                        Bad c2 x () ->
                            if c1 || c2 then
                                Bad True x ()

                            else
                                Good False fallback s0

                        Good c2 b s2 ->
                            case parseC s2 of
                                Bad c3 x () ->
                                    if c1 || c2 || c3 then
                                        Bad True x ()

                                    else
                                        Good False fallback s0

                                Good c3 c s3 ->
                                    case parseD s3 of
                                        Bad c4 x () ->
                                            if c1 || c2 || c3 || c4 then
                                                Bad True x ()

                                            else
                                                Good False fallback s0

                                        Good c4 d s4 ->
                                            Good (c1 || c2 || c3 || c4) (func a b c d) s4
        )


oneOf2Map :
    (first -> choice)
    -> Parser first
    -> (second -> choice)
    -> Parser second
    -> Parser choice
oneOf2Map firstToChoice (Parser attemptFirst) secondToChoice (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                Good firstCommitted first s1 ->
                    Good firstCommitted (firstToChoice first) s1

                Bad firstCommitted firstX () ->
                    if firstCommitted then
                        Bad firstCommitted firstX ()

                    else
                        case attemptSecond s of
                            Good secondCommitted second s1 ->
                                Good secondCommitted (secondToChoice second) s1

                            Bad secondCommitted secondX () ->
                                if secondCommitted then
                                    Bad secondCommitted secondX ()

                                else
                                    Bad False (Append firstX secondX) ()
        )


oneOf2 : Parser a -> Parser a -> Parser a
oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Bad False (Append firstX secondX) ()
        )


oneOf2OrSucceed : Parser a -> Parser a -> a -> Parser a
oneOf2OrSucceed (Parser attemptFirst) (Parser attemptSecond) thirdRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted _ ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted _ ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    Good False thirdRes s
        )


oneOf3 : Parser a -> Parser a -> Parser a -> Parser a
oneOf3 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX ()) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                Bad False (Append firstX (Append secondX thirdX)) ()
        )


oneOf4 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf4 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) (Parser attemptFourth) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX ()) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                case attemptFourth s of
                                                    (Good _ _ _) as fourthGood ->
                                                        fourthGood

                                                    (Bad fourthCommitted fourthX ()) as fourthBad ->
                                                        if fourthCommitted then
                                                            fourthBad

                                                        else
                                                            Bad False (Append firstX (Append secondX (Append thirdX fourthX))) ()
        )


oneOf5 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf5 (Parser attemptFirst) (Parser attemptSecond) (Parser attemptThird) (Parser attemptFourth) (Parser attemptFifth) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstGood ->
                    firstGood

                (Bad firstCommitted firstX ()) as firstBad ->
                    if firstCommitted then
                        firstBad

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondGood ->
                                secondGood

                            (Bad secondCommitted secondX ()) as secondBad ->
                                if secondCommitted then
                                    secondBad

                                else
                                    case attemptThird s of
                                        (Good _ _ _) as thirdGood ->
                                            thirdGood

                                        (Bad thirdCommitted thirdX ()) as thirdBad ->
                                            if thirdCommitted then
                                                thirdBad

                                            else
                                                case attemptFourth s of
                                                    (Good _ _ _) as fourthGood ->
                                                        fourthGood

                                                    (Bad fourthCommitted fourthX ()) as fourthBad ->
                                                        if fourthCommitted then
                                                            fourthBad

                                                        else
                                                            case attemptFifth s of
                                                                (Good _ _ _) as fifthGood ->
                                                                    fifthGood

                                                                (Bad fifthCommitted fifthX ()) as fifthBad ->
                                                                    if fifthCommitted then
                                                                        fifthBad

                                                                    else
                                                                        Bad False (Append firstX (Append secondX (Append thirdX (Append fourthX fifthX)))) ()
        )


oneOf7 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf7 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _ _) as good ->
                    good

                (Bad committed0 x0 ()) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _ _) as good ->
                                good

                            (Bad committed1 x1 ()) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _ _) as good ->
                                            good

                                        (Bad committed2 x2 ()) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _ _) as good ->
                                                        good

                                                    (Bad committed3 x3 ()) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4 ()) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5 ()) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6 ()) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                Bad False (Append x0 (Append x1 (Append x2 (Append x3 (Append x4 (Append x5 x6)))))) ()
        )


oneOf10 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf10 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _ _) as good ->
                    good

                (Bad committed0 x0 ()) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _ _) as good ->
                                good

                            (Bad committed1 x1 ()) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _ _) as good ->
                                            good

                                        (Bad committed2 x2 ()) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _ _) as good ->
                                                        good

                                                    (Bad committed3 x3 ()) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4 ()) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5 ()) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6 ()) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7 ()) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8 ()) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9 ()) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    Bad False (Append x0 (Append x1 (Append x2 (Append x3 (Append x4 (Append x5 (Append x6 (Append x7 (Append x8 x9))))))))) ()
        )


oneOf14 : Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
oneOf14 (Parser attempt0) (Parser attempt1) (Parser attempt2) (Parser attempt3) (Parser attempt4) (Parser attempt5) (Parser attempt6) (Parser attempt7) (Parser attempt8) (Parser attempt9) (Parser attempt10) (Parser attempt11) (Parser attempt12) (Parser attempt13) =
    Parser
        (\s ->
            case attempt0 s of
                (Good _ _ _) as good ->
                    good

                (Bad committed0 x0 ()) as bad0 ->
                    if committed0 then
                        bad0

                    else
                        case attempt1 s of
                            (Good _ _ _) as good ->
                                good

                            (Bad committed1 x1 ()) as bad1 ->
                                if committed1 then
                                    bad1

                                else
                                    case attempt2 s of
                                        (Good _ _ _) as good ->
                                            good

                                        (Bad committed2 x2 ()) as bad2 ->
                                            if committed2 then
                                                bad2

                                            else
                                                case attempt3 s of
                                                    (Good _ _ _) as good ->
                                                        good

                                                    (Bad committed3 x3 ()) as bad3 ->
                                                        if committed3 then
                                                            bad3

                                                        else
                                                            case attempt4 s of
                                                                (Good _ _ _) as good ->
                                                                    good

                                                                (Bad committed4 x4 ()) as bad4 ->
                                                                    if committed4 then
                                                                        bad4

                                                                    else
                                                                        case attempt5 s of
                                                                            (Good _ _ _) as good ->
                                                                                good

                                                                            (Bad committed5 x5 ()) as bad5 ->
                                                                                if committed5 then
                                                                                    bad5

                                                                                else
                                                                                    case attempt6 s of
                                                                                        (Good _ _ _) as good ->
                                                                                            good

                                                                                        (Bad committed6 x6 ()) as bad6 ->
                                                                                            if committed6 then
                                                                                                bad6

                                                                                            else
                                                                                                case attempt7 s of
                                                                                                    (Good _ _ _) as good ->
                                                                                                        good

                                                                                                    (Bad committed7 x7 ()) as bad7 ->
                                                                                                        if committed7 then
                                                                                                            bad7

                                                                                                        else
                                                                                                            case attempt8 s of
                                                                                                                (Good _ _ _) as good ->
                                                                                                                    good

                                                                                                                (Bad committed8 x8 ()) as bad8 ->
                                                                                                                    if committed8 then
                                                                                                                        bad8

                                                                                                                    else
                                                                                                                        case attempt9 s of
                                                                                                                            (Good _ _ _) as good ->
                                                                                                                                good

                                                                                                                            (Bad committed9 x9 ()) as bad9 ->
                                                                                                                                if committed9 then
                                                                                                                                    bad9

                                                                                                                                else
                                                                                                                                    case attempt10 s of
                                                                                                                                        (Good _ _ _) as good ->
                                                                                                                                            good

                                                                                                                                        (Bad committed10 x10 ()) as bad10 ->
                                                                                                                                            if committed10 then
                                                                                                                                                bad10

                                                                                                                                            else
                                                                                                                                                case attempt11 s of
                                                                                                                                                    (Good _ _ _) as good ->
                                                                                                                                                        good

                                                                                                                                                    (Bad committed11 x11 ()) as bad11 ->
                                                                                                                                                        if committed11 then
                                                                                                                                                            bad11

                                                                                                                                                        else
                                                                                                                                                            case attempt12 s of
                                                                                                                                                                (Good _ _ _) as good ->
                                                                                                                                                                    good

                                                                                                                                                                (Bad committed12 x12 ()) as bad12 ->
                                                                                                                                                                    if committed12 then
                                                                                                                                                                        bad12

                                                                                                                                                                    else
                                                                                                                                                                        case attempt13 s of
                                                                                                                                                                            (Good _ _ _) as good ->
                                                                                                                                                                                good

                                                                                                                                                                            (Bad committed13 x13 ()) as bad13 ->
                                                                                                                                                                                if committed13 then
                                                                                                                                                                                    bad13

                                                                                                                                                                                else
                                                                                                                                                                                    Bad False (Append x0 (Append x1 (Append x2 (Append x3 (Append x4 (Append x5 (Append x6 (Append x7 (Append x8 (Append x9 (Append x10 (Append x11 (Append x12 x13))))))))))))) ()
        )


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
    case possibilities of
        [] ->
            Parser (\s -> Bad False (fromState s problemOnEmptyPossibilityList) ())

        (Parser parse) :: remainingParsers ->
            Parser
                (\s ->
                    case parse s of
                        (Good _ _ _) as step ->
                            step

                        (Bad committed x ()) as step ->
                            if committed then
                                step

                            else
                                oneOfHelp s x remainingParsers
                )


problemOnEmptyPossibilityList : Parser.Problem
problemOnEmptyPossibilityList =
    Parser.Problem "empty oneOf"


oneOfHelp : State -> RopeFilled (DeadEnd Parser.Problem) -> List (Parser a) -> PStep Parser.Problem a
oneOfHelp s0 deadEnds parsers =
    case parsers of
        [] ->
            Bad False deadEnds ()

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad committed x ()) as step ->
                    if committed then
                        step

                    else
                        oneOfHelp s0 (Append deadEnds x) remainingParsers


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
map func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (func a) s1

                Bad committed x () ->
                    Bad committed x ()
        )


loopWhileSucceeds : Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser res
loopWhileSucceeds element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopWhileSucceedsHelp False element initialFolded reduce foldedToRes s)


loopWhileSucceedsHelp : Bool -> Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep Parser.Problem res
loopWhileSucceedsHelp committedSoFar ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseElement s0 of
        Good elementCommitted elementResult s1 ->
            loopWhileSucceedsHelp (committedSoFar || elementCommitted)
                element
                (soFar |> reduce elementResult)
                reduce
                foldedToRes
                s1

        Bad elementCommitted x () ->
            if elementCommitted then
                Bad True x ()

            else
                Good committedSoFar (foldedToRes soFar) s0


loopUntil : Parser () -> Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> Parser res
loopUntil endParser element initialFolded reduce foldedToRes =
    Parser
        (\s -> loopUntilHelp False endParser element initialFolded reduce foldedToRes s)


loopUntilHelp : Bool -> Parser () -> Parser element -> folded -> (element -> folded -> folded) -> (folded -> res) -> State -> PStep Parser.Problem res
loopUntilHelp committedSoFar ((Parser parseEnd) as endParser) ((Parser parseElement) as element) soFar reduce foldedToRes s0 =
    case parseEnd s0 of
        Good endCommitted () s1 ->
            Good (committedSoFar || endCommitted) (foldedToRes soFar) s1

        Bad endCommitted endX () ->
            if endCommitted then
                Bad True endX ()

            else
                case parseElement s0 of
                    Good elementCommitted elementResult s1 ->
                        loopUntilHelp (committedSoFar || elementCommitted)
                            endParser
                            element
                            (soFar |> reduce elementResult)
                            reduce
                            foldedToRes
                            s1

                    Bad elementCommitted x () ->
                        Bad (committedSoFar || elementCommitted) x ()


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


numberHelp :
    { int : Result Parser.Problem (Int -> a)
    , hex : Result Parser.Problem (Int -> a)
    , octal : Result Parser.Problem (Int -> a)
    , binary : Result Parser.Problem (Int -> a)
    , float : Result Parser.Problem (Float -> a)
    , invalid : Parser.Problem
    , expecting : Parser.Problem
    }
    -> Parser a
numberHelp consumers =
    let
        parserAdvancedNumberAndStringLength : Parser.Advanced.Parser c Parser.Problem { length : Int, number : a }
        parserAdvancedNumberAndStringLength =
            Parser.Advanced.map (\n -> \endOffset -> { length = endOffset, number = n })
                (Parser.Advanced.number consumers)
                |= Parser.Advanced.getOffset
    in
    Parser
        (\state ->
            if String.any Char.isDigit (String.slice state.offset (state.offset + 1) state.src) then
                case Parser.Advanced.run parserAdvancedNumberAndStringLength (String.slice state.offset (String.length state.src) state.src) of
                    Ok result ->
                        Good False result.number (stateAddLengthToOffsetAndColumn result.length state)

                    Err _ ->
                        Bad False (fromState state Parser.ExpectingNumber) ()

            else
                Bad False (fromState state Parser.ExpectingNumber) ()
        )


stateAddLengthToOffsetAndColumn : Int -> State -> State
stateAddLengthToOffsetAndColumn lengthAdded s =
    { src = s.src
    , offset = s.offset + lengthAdded
    , indent = s.indent
    , row = s.row
    , col = s.col + lengthAdded
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


floatOrIntOrHex : (Float -> a) -> (Int -> a) -> (Int -> a) -> Parser a
floatOrIntOrHex floatf intf hexf =
    numberHelp
        { int = Ok intf
        , hex = Ok hexf
        , octal = Err Parser.ExpectingNumber
        , binary = Err Parser.ExpectingNumber
        , float = Ok floatf
        , invalid = Parser.ExpectingNumber
        , expecting = Parser.ExpectingNumber
        }


intOrHex : (Int -> a) -> (Int -> a) -> Parser a
intOrHex intf hexf =
    numberHelp
        { int = Ok intf
        , hex = Ok hexf
        , octal = Err Parser.ExpectingNumber
        , binary = Err Parser.ExpectingNumber
        , float = Err Parser.ExpectingNumber
        , invalid = Parser.ExpectingNumber
        , expecting = Parser.ExpectingNumber
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
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + strLength
            in
            if String.slice s.offset newOffset s.src == str ++ "" then
                Good True
                    res
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + strLength
                    }

            else
                Bad False (fromState s expecting) ()
        )


followedBySymbol : String -> Parser a -> Parser a
followedBySymbol str (Parser parsePrevious) =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s0 ->
            case parsePrevious s0 of
                Good previousCommitted res s1 ->
                    let
                        newOffset : Int
                        newOffset =
                            s1.offset + strLength
                    in
                    if String.slice s1.offset newOffset s1.src == str ++ "" then
                        Good True
                            res
                            { src = s1.src
                            , offset = newOffset
                            , indent = s1.indent
                            , row = s1.row
                            , col = s1.col + strLength
                            }

                    else
                        Bad previousCommitted (fromState s1 expecting) ()

                bad ->
                    bad
        )


{-| Make sure the given String does not contain \\n
or 2-part UTF-16 characters.
-}
symbolBacktrackable : String -> res -> Parser res
symbolBacktrackable str res =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + strLength
            in
            if String.slice s.offset newOffset s.src == str ++ "" then
                Good False
                    res
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + strLength
                    }

            else
                Bad False (fromState s expecting) ()
        )


symbolWithEndPosition : String -> (Location -> res) -> Parser res
symbolWithEndPosition str endPositionToRes =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + strLength
            in
            if String.slice s.offset newOffset s.src == str ++ "" then
                let
                    newCol : Int
                    newCol =
                        s.col + strLength
                in
                Good True
                    (endPositionToRes { row = s.row, column = newCol })
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = newCol
                    }

            else
                Bad False (fromState s expecting) ()
        )


symbolWithStartAndEndPosition : String -> (Location -> Location -> res) -> Parser res
symbolWithStartAndEndPosition str startAndEndPositionToRes =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + strLength
            in
            if String.slice s.offset newOffset s.src == str ++ "" then
                let
                    newCol : Int
                    newCol =
                        s.col + strLength
                in
                Good True
                    (startAndEndPositionToRes { row = s.row, column = s.col } { row = s.row, column = newCol })
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = newCol
                    }

            else
                Bad False (fromState s expecting) ()
        )


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
symbolFollowedBy : String -> Parser next -> Parser next
symbolFollowedBy str (Parser parseNext) =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingSymbol str

        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + strLength
            in
            if String.slice s.offset newOffset s.src == str ++ "" then
                parseNext
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + strLength
                    }
                    |> pStepCommit

            else
                Bad False (fromState s expecting) ()
        )


pStepCommit : PStep x a -> PStep x a
pStepCommit pStep =
    case pStep of
        Good _ a state ->
            Good True a state

        Bad _ errors () ->
            Bad True errors ()


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
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingKeyword kwd

        kwdLength : Int
        kwdLength =
            String.length kwd
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + kwdLength
            in
            if
                (String.slice s.offset newOffset s.src == kwd ++ "")
                    && not (isSubCharAlphaNumOrUnderscore newOffset s.src)
            then
                Good True
                    res
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + kwdLength
                    }

            else
                Bad False (fromState s expecting) ()
        )


isSubCharAlphaNumOrUnderscore : Int -> String -> Bool
isSubCharAlphaNumOrUnderscore offset string =
    String.any (\c -> Char.Extra.isAlphaNumFast c || c == '_')
        (String.slice offset (offset + 1) string)


{-| Make sure the given String isn't empty and does not contain \\n
or 2-part UTF-16 characters.
-}
keywordFollowedBy : String -> Parser next -> Parser next
keywordFollowedBy kwd (Parser parseNext) =
    let
        expecting : Parser.Problem
        expecting =
            Parser.ExpectingKeyword kwd

        kwdLength : Int
        kwdLength =
            String.length kwd
    in
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    s.offset + kwdLength
            in
            if
                (String.slice s.offset newOffset s.src == kwd ++ "")
                    && not (isSubCharAlphaNumOrUnderscore newOffset s.src)
            then
                parseNext
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + kwdLength
                    }
                    |> pStepCommit

            else
                Bad False (fromState s expecting) ()
        )


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
    Parser
        (\s ->
            if String.length s.src == s.offset + 0 then
                Good False () s

            else
                Bad False (fromState s Parser.ExpectingEnd) ()
        )


anyChar : Parser Char
anyChar =
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    charOrEnd s.offset s.src
            in
            if newOffset == -1 then
                -- end of source
                Bad False (fromState s Parser.UnexpectedChar) ()

            else if newOffset == -2 then
                -- newline
                Good True
                    '\n'
                    { src = s.src
                    , offset = s.offset + 1
                    , indent = s.indent
                    , row = s.row + 1
                    , col = 1
                    }

            else
                -- found
                case String.toList (String.slice s.offset newOffset s.src) of
                    [] ->
                        Bad False (fromState s Parser.UnexpectedChar) ()

                    c :: _ ->
                        Good True
                            c
                            { src = s.src
                            , offset = newOffset
                            , indent = s.indent
                            , row = s.row
                            , col = s.col + 1
                            }
        )


charOrEnd : Int -> String -> Int
charOrEnd offset string =
    let
        actualChar : String
        actualChar =
            String.slice offset (offset + 1) string
    in
    case actualChar of
        "\n" ->
            -2

        "" ->
            -1

        _ ->
            if charStringIsUtf16HighSurrogate actualChar then
                offset + 2

            else
                offset + 1


charStringIsUtf16HighSurrogate : String -> Bool
charStringIsUtf16HighSurrogate charString =
    charString |> String.any (\c -> Basics.isNaN (Basics.toFloat (Char.toCode c)))


whileMap : (Char -> Bool) -> (String -> res) -> Parser res
whileMap isGood chompedStringToRes =
    Parser
        (\s0 ->
            let
                s1 : State
                s1 =
                    chompWhileHelp isGood s0.offset s0.row s0.col s0.src s0.indent
            in
            Good (s1.offset > s0.offset)
                (chompedStringToRes (String.slice s0.offset s1.offset s0.src))
                s1
        )


chompWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
chompWhileHelp isGood offset row col src indent =
    let
        actualChar : String
        actualChar =
            String.slice offset (offset + 1) src
    in
    if String.any isGood actualChar then
        case actualChar of
            "\n" ->
                chompWhileHelp isGood (offset + 1) (row + 1) 1 src indent

            _ ->
                chompWhileHelp isGood (offset + 1) row (col + 1) src indent

    else if
        charStringIsUtf16HighSurrogate actualChar
            && -- String.any iterates over code points (so here just one Char)
               String.any isGood (String.slice offset (offset + 2) src)
    then
        chompWhileHelp isGood (offset + 2) row (col + 1) src indent

    else
        -- no match
        { src = src
        , offset = offset
        , indent = indent
        , row = row
        , col = col
        }


chompWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
chompWhileWithoutLinebreakHelp isGood offset row col src indent =
    let
        actualChar : String
        actualChar =
            String.slice offset (offset + 1) src
    in
    if String.any isGood actualChar then
        chompWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

    else if
        charStringIsUtf16HighSurrogate actualChar
            && -- String.any iterates over code points (so here just one Char)
               String.any isGood (String.slice offset (offset + 2) src)
    then
        chompWhileWithoutLinebreakHelp isGood (offset + 2) row (col + 1) src indent

    else
        -- no match
        { src = src
        , offset = offset
        , indent = indent
        , row = row
        , col = col
        }


{-| Specialized `chompWhile (\c -> c == " " || c == "\n" || c == "\r")`
optimized for speed
-}
chompWhileWhitespaceFollowedBy : Parser next -> Parser next
chompWhileWhitespaceFollowedBy (Parser parseNext) =
    Parser
        (\s0 ->
            let
                s1 : State
                s1 =
                    chompWhileWhitespaceHelp s0.offset s0.row s0.col s0.src s0.indent
            in
            if s1.offset > s0.offset then
                parseNext s1
                    |> pStepCommit

            else
                parseNext s1
        )


chompWhileWhitespaceHelp : Int -> Int -> Int -> String -> Int -> State
chompWhileWhitespaceHelp offset row col src indent =
    case String.slice offset (offset + 1) src of
        " " ->
            chompWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

        "\n" ->
            chompWhileWhitespaceHelp (offset + 1) (row + 1) 1 src indent

        "\u{000D}" ->
            chompWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

        -- empty or non-whitespace
        _ ->
            { src = src, offset = offset, indent = indent, row = row, col = col }


{-| Some languages are indentation sensitive. Python cares about tabs. Elm
cares about spaces sometimes. `withIndent` and `getIndent` allow you to manage
"indentation state" yourself, however is necessary in your scenario.

@test-helper

-}
withIndent : Int -> Parser a -> Parser a
withIndent newIndent (Parser parse) =
    Parser
        (\s0 ->
            case parse (changeIndent newIndent s0) of
                Good committed a s1 ->
                    Good committed a (changeIndent s0.indent s1)

                bad ->
                    bad
        )


changeIndent : Int -> State -> State
changeIndent newIndent s =
    { src = s.src
    , offset = s.offset
    , indent = newIndent
    , row = s.row
    , col = s.col
    }


{-| For a given ParserWithComments.Parser, take the current start column as indentation for the whole block
-}
withIndentSetToColumn : Parser a -> Parser a
withIndentSetToColumn (Parser parse) =
    Parser
        (\s0 ->
            case parse (changeIndent s0.col s0) of
                Good committed a s1 ->
                    Good committed a (changeIndent s0.indent s1)

                bad ->
                    bad
        )


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
mapWithStartAndEndPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good committed a s1 ->
                    Good committed (combineStartAndResult { row = s0.row, column = s0.col } a { row = s1.row, column = s1.col }) s1

                Bad committed x () ->
                    Bad committed x ()
        )


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


ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak :
    (Location -> String -> Location -> res)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> Set.Set String
    -> Parser res
ifFollowedByWhileExceptMapWithStartAndEndPositionsWithoutLinebreak toResult firstIsOkay afterFirstIsOkay exceptionSet =
    Parser
        (\s0 ->
            let
                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s0.offset s0.src
            in
            if firstOffset == -1 then
                Bad False (fromState s0 Parser.ExpectingVariable) ()

            else
                let
                    s1 : State
                    s1 =
                        chompWhileWithoutLinebreakHelp afterFirstIsOkay firstOffset s0.row (s0.col + 1) s0.src s0.indent

                    name : String
                    name =
                        String.slice s0.offset s1.offset s0.src
                in
                if Set.member name exceptionSet then
                    Bad False (fromState s0 Parser.ExpectingVariable) ()

                else
                    Good True (toResult { row = s0.row, column = s0.col } name { row = s1.row, column = s1.col }) s1
        )


ifFollowedByWhileWithoutLinebreak :
    (Char -> Bool)
    -> (Char -> Bool)
    -> Parser String
ifFollowedByWhileWithoutLinebreak firstIsOkay afterFirstIsOkay =
    Parser
        (\s ->
            let
                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s.offset s.src
            in
            if firstOffset == -1 then
                Bad False (fromState s Parser.UnexpectedChar) ()

            else
                let
                    s1 : State
                    s1 =
                        chompWhileWithoutLinebreakHelp afterFirstIsOkay firstOffset s.row (s.col + 1) s.src s.indent
                in
                Good True (String.slice s.offset s1.offset s.src) s1
        )


ifFollowedByWhileMapWithStartAndEndPositionWithoutLinebreak :
    (Location -> String -> Location -> res)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> Parser res
ifFollowedByWhileMapWithStartAndEndPositionWithoutLinebreak rangeAndChompedToRes firstIsOkay afterFirstIsOkay =
    Parser
        (\s0 ->
            let
                firstOffset : Int
                firstOffset =
                    isSubCharWithoutLinebreak firstIsOkay s0.offset s0.src
            in
            if firstOffset == -1 then
                Bad False (fromState s0 Parser.UnexpectedChar) ()

            else
                let
                    s1 : State
                    s1 =
                        chompWhileWithoutLinebreakHelp afterFirstIsOkay firstOffset s0.row (s0.col + 1) s0.src s0.indent
                in
                Good True
                    (rangeAndChompedToRes
                        { row = s0.row, column = s0.col }
                        (String.slice s0.offset s1.offset s0.src)
                        { row = s1.row, column = s1.col }
                    )
                    s1
        )


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
nestableMultiComment : ( Char, String ) -> ( Char, String ) -> Parser String
nestableMultiComment ( openChar, openTail ) ( closeChar, closeTail ) =
    let
        open : String
        open =
            String.cons openChar openTail

        close : String
        close =
            String.cons closeChar closeTail

        isNotRelevant : Char -> Bool
        isNotRelevant char =
            char /= openChar && char /= closeChar
    in
    map2
        (\afterOpen contentAfterAfterOpen ->
            open ++ afterOpen ++ contentAfterAfterOpen ++ close
        )
        (symbolFollowedBy open
            (while isNotRelevant)
        )
        (oneOf2
            (symbol close "")
            (loop
                ( "", 1 )
                (oneOf3
                    (symbol close ( close, -1 ))
                    (symbol open ( open, 1 ))
                    (anyCharFollowedByWhileMap (\chomped -> ( chomped, 0 ))
                        isNotRelevant
                    )
                )
                (\( toAppend, nestingChange ) ( soFarContent, soFarNesting ) ->
                    let
                        newNesting : Int
                        newNesting =
                            soFarNesting + nestingChange
                    in
                    if newNesting == 0 then
                        Done soFarContent

                    else
                        Loop ( soFarContent ++ toAppend ++ "", newNesting )
                )
            )
        )


while : (Char -> Bool) -> Parser String
while isGood =
    Parser
        (\s0 ->
            let
                s1 : State
                s1 =
                    chompWhileHelp isGood s0.offset s0.row s0.col s0.src s0.indent
            in
            Good (s1.offset > s0.offset)
                (String.slice s0.offset s1.offset s0.src)
                s1
        )


whileWithoutLinebreak : (Char -> Bool) -> Parser String
whileWithoutLinebreak isGood =
    Parser
        (\s0 ->
            let
                s1 : State
                s1 =
                    chompWhileWithoutLinebreakHelp isGood s0.offset s0.row s0.col s0.src s0.indent
            in
            Good (s1.offset > s0.offset)
                (String.slice s0.offset s1.offset s0.src)
                s1
        )


anyCharFollowedByWhileMap :
    (String -> res)
    -> (Char -> Bool)
    -> Parser res
anyCharFollowedByWhileMap chompedStringToRes afterFirstIsOkay =
    Parser
        (\s ->
            let
                firstOffset : Int
                firstOffset =
                    charOrEnd s.offset s.src
            in
            if firstOffset == -1 then
                -- end of source
                Bad False (fromState s expectingAnyChar) ()

            else
                let
                    s1 : State
                    s1 =
                        if firstOffset == -2 then
                            chompWhileHelp afterFirstIsOkay (s.offset + 1) (s.row + 1) 1 s.src s.indent

                        else
                            chompWhileHelp afterFirstIsOkay firstOffset s.row (s.col + 1) s.src s.indent
                in
                Good True (chompedStringToRes (String.slice s.offset s1.offset s.src)) s1
        )


expectingAnyChar : Parser.Problem
expectingAnyChar =
    Parser.Problem "any char"


{-| Decide what steps to take next in your `loop`.

If you are `Done`, you give the result of the whole `loop`. If you decide to
`Loop` around again, you give a new state to work from. Maybe you need to add
an item to a list? Or maybe you need to track some information about what you
just saw?

**Note:** It may be helpful to learn about [finite-state machines][fsm] to get
a broader intuition about using `state`. I.e. You may want to create a `type`
that describes four possible states, and then use `Loop` to transition between
them as you consume characters.

[fsm]: https://en.wikipedia.org/wiki/Finite-state_machine

-}
type Step state a
    = Loop state
    | Done a


{-| A parser that can loop indefinitely. This can be helpful when parsing
repeated structures, like a bunch of statements:


    statements : Parser (List Stmt)
    statements =
        loop [] statementsHelp

    statementsHelp : List Stmt -> Parser (Step (List Stmt) (List Stmt))
    statementsHelp revStmts =
        oneOf
            [ succeed (\stmt -> Loop (stmt :: revStmts))
                |= statement
                |> ParserFast.ignore spaces
                |> ParserFast.ignore symbol ";"
                |> ParserFast.ignore spaces
            , succeed ()
                |> map (\_ -> Done (List.reverse revStmts))
            ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to e.g.
use an [`ifFollowedByWhileWithoutLinebreak`](#ifFollowedByWhileWithoutLinebreak) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.

-}
loop : state -> Parser extension -> (extension -> state -> Step state a) -> Parser a
loop state element reduce =
    Parser
        (\s -> loopHelp False state element reduce s)


loopHelp : Bool -> state -> Parser extension -> (extension -> state -> Step state a) -> State -> PStep Parser.Problem a
loopHelp committedSoFar state ((Parser parseElement) as element) reduce s0 =
    case parseElement s0 of
        Good elementCommitted step s1 ->
            case reduce step state of
                Loop newState ->
                    loopHelp (committedSoFar || elementCommitted) newState element reduce s1

                Done result ->
                    Good (committedSoFar || elementCommitted) result s1

        Bad elementCommitted x () ->
            Bad (committedSoFar || elementCommitted) x ()


{-| When parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubCharWithoutLinebreak isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

-}
isSubCharWithoutLinebreak : (Char -> Bool) -> Int -> String -> Int
isSubCharWithoutLinebreak predicate offset string =
    -- https://github.com/elm/parser/blob/1.1.0/src/Elm/Kernel/Parser.js#L37
    let
        actualChar : String
        actualChar =
            String.slice offset (offset + 1) string
    in
    if String.any predicate actualChar then
        offset + 1

    else if
        charStringIsUtf16HighSurrogate actualChar
            && -- String.any iterates over code points (so here just one Char)
               String.any predicate (String.slice offset (offset + 2) string)
    then
        offset + 2

    else
        -1
