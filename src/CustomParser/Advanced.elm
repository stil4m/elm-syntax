module CustomParser.Advanced exposing
    ( Parser, run
    , number, symbol, symbolFollowedBy, keyword, keywordFollowedBy, variable, end
    , succeed, problem, succeedLazy, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, andThen, ignore
    , orSucceed, orSucceedLazy, oneOf2, oneOf, backtrackable, commit
    , loop, Step(..)
    , nestableMultiComment
    , getChompedString, chompIf, chompWhile, mapChompedString
    , withIndent
    , columnAndThen, columnIndentAndThen, offsetSourceAndThen, mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition
    )

{-|

@docs Parser, run

@docs number, symbol, symbolFollowedBy, keyword, keywordFollowedBy, variable, end


# Flow

@docs succeed, problem, succeedLazy, lazy, map, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, andThen, ignore

@docs orSucceed, orSucceedLazy, oneOf2, oneOf, backtrackable, commit

@docs loop, Step


# Whitespace

@docs nestableMultiComment


# Chompers

@docs getChompedString, chompIf, chompWhile, mapChompedString


# Indentation

@docs withIndent


# Positions

@docs columnAndThen, columnIndentAndThen, offsetSourceAndThen, mapWithStartPosition, mapWithEndPosition, mapWithStartAndEndPosition

-}

import Char
import Char.Extra
import Parser.Advanced exposing ((|=))
import Set



-- PARSERS


{-| An advanced `CustomParser` gives a way to improve your error messages: `problem`;
Instead of all errors being a `String`, you can create a
custom type like `type Problem = BadIndent | BadKeyword String` and track
problems much more precisely.

I recommend starting with the simpler [`CustomParser`](CustomParser) module though, and
when you feel comfortable and want better error messages, you can create a type
alias like this:

    import Parser.Advanced

    type alias MyParser a =
        Parser.Advanced.Parser Context Problem a

    type Context
        = Definition String
        | List
        | Record

    type Problem
        = BadIndent
        | BadKeyword String

All of the functions from `CustomParser` should exist in `CustomParser.Advanced` in some
form, allowing you to switch over pretty easily.

-}
type Parser problem value
    = Parser (State -> PStep problem value)


type PStep problem value
    = Good Bool value State
    | Bad Bool (Bag problem)


type alias State =
    { src : String
    , offset : Int
    , indent : Int
    , row : Int
    , col : Int
    }



-- RUN


{-| This works just like [`CustomParser.run`](CustomParser#run).
The only difference is that when it fails, it has much more precise information
for each dead end.
-}
run : Parser x a -> String -> Result (List (DeadEnd x)) a
run (Parser parse) src =
    case parse { src = src, offset = 0, indent = 1, row = 1, col = 1 } of
        Good _ value _ ->
            Ok value

        Bad _ bag ->
            Err (bagToList bag [])



-- PROBLEMS


{-| Say you are parsing a function named `viewHealthData` that contains a list.
You might get a `DeadEnd` like this:

    { row = 18
    , col = 22
    , problem = UnexpectedComma
    , contextStack =
        [ { row = 14
          , col = 1
          , context = Definition "viewHealthData"
          }
        , { row = 15
          , col = 4
          , context = List
          }
        ]
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


type Bag x
    = Empty
    | AddRight (Bag x) (DeadEnd x)
    | Append (Bag x) (Bag x)


fromState : State -> x -> Bag x
fromState s x =
    AddRight Empty (DeadEnd s.row s.col x)


bagToList : Bag x -> List (DeadEnd x) -> List (DeadEnd x)
bagToList bag list =
    case bag of
        Empty ->
            list

        AddRight bag1 x ->
            bagToList bag1 (x :: list)

        Append bag1 bag2 ->
            bagToList bag1 (bagToList bag2 list)


succeedLazy : (() -> a) -> Parser x a
succeedLazy res =
    Parser (\s -> Good False (res ()) s)


succeed : a -> Parser x a
succeed a =
    Parser (\s -> Good False a s)


problem : x -> Parser x a
problem x =
    Parser (\s -> Bad False (fromState s x))


map : (a -> b) -> Parser x a -> Parser x b
map func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (func a) s1

                Bad p x ->
                    Bad p x
        )


map2 : (a -> b -> value) -> Parser x a -> Parser x b -> Parser x value
map2 func (Parser parseA) (Parser parseB) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) (func a b) s2
        )


map3 : (a -> b -> c -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x value
map3 func (Parser parseA) (Parser parseB) (Parser parseC) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    Good (p1 || p2 || p3) (func a b c) s3
        )


map4 : (a -> b -> c -> d -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x value
map4 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            Good (p1 || p2 || p3 || p4) (func a b c d) s4
        )


map5 : (a -> b -> c -> d -> e -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x value
map5 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    Good (p1 || p2 || p3 || p4 || p5) (func a b c d e) s5
        )


map6 : (a -> b -> c -> d -> e -> f -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x value
map6 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            Good (p1 || p2 || p3 || p4 || p5 || p6) (func a b c d e f) s6
        )


map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x value
map7 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            case parseG s6 of
                                                                Bad p7 x ->
                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7) x

                                                                Good p7 g s7 ->
                                                                    Good (p1 || p2 || p3 || p4 || p5 || p6 || p7) (func a b c d e f g) s7
        )


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x value
map8 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            case parseG s6 of
                                                                Bad p7 x ->
                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7) x

                                                                Good p7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad p8 x ->
                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8) x

                                                                        Good p8 h s8 ->
                                                                            Good (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8) (func a b c d e f g h) s8
        )


map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x i -> Parser x value
map9 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            case parseG s6 of
                                                                Bad p7 x ->
                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7) x

                                                                Good p7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad p8 x ->
                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8) x

                                                                        Good p8 h s8 ->
                                                                            case parseI s8 of
                                                                                Bad p9 x ->
                                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9) x

                                                                                Good p9 i s9 ->
                                                                                    Good (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9) (func a b c d e f g h i) s9
        )


map10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x i -> Parser x j -> Parser x value
map10 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) (Parser parseJ) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            case parseG s6 of
                                                                Bad p7 x ->
                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7) x

                                                                Good p7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad p8 x ->
                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8) x

                                                                        Good p8 h s8 ->
                                                                            case parseI s8 of
                                                                                Bad p9 x ->
                                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9) x

                                                                                Good p9 i s9 ->
                                                                                    case parseJ s9 of
                                                                                        Bad p10 x ->
                                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9 || p10) x

                                                                                        Good p10 j s10 ->
                                                                                            Good (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9 || p10) (func a b c d e f g h i j) s10
        )


map11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> value) -> Parser x a -> Parser x b -> Parser x c -> Parser x d -> Parser x e -> Parser x f -> Parser x g -> Parser x h -> Parser x i -> Parser x j -> Parser x k -> Parser x value
map11 func (Parser parseA) (Parser parseB) (Parser parseC) (Parser parseD) (Parser parseE) (Parser parseF) (Parser parseG) (Parser parseH) (Parser parseI) (Parser parseJ) (Parser parseK) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            case parseC s2 of
                                Bad p3 x ->
                                    Bad (p1 || p2 || p3) x

                                Good p3 c s3 ->
                                    case parseD s3 of
                                        Bad p4 x ->
                                            Bad (p1 || p2 || p3 || p4) x

                                        Good p4 d s4 ->
                                            case parseE s4 of
                                                Bad p5 x ->
                                                    Bad (p1 || p2 || p3 || p4 || p5) x

                                                Good p5 e s5 ->
                                                    case parseF s5 of
                                                        Bad p6 x ->
                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6) x

                                                        Good p6 f s6 ->
                                                            case parseG s6 of
                                                                Bad p7 x ->
                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7) x

                                                                Good p7 g s7 ->
                                                                    case parseH s7 of
                                                                        Bad p8 x ->
                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8) x

                                                                        Good p8 h s8 ->
                                                                            case parseI s8 of
                                                                                Bad p9 x ->
                                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9) x

                                                                                Good p9 i s9 ->
                                                                                    case parseJ s9 of
                                                                                        Bad p10 x ->
                                                                                            Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9 || p10) x

                                                                                        Good p10 j s10 ->
                                                                                            case parseK s10 of
                                                                                                Bad p11 x ->
                                                                                                    Bad (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9 || p10 || p11) x

                                                                                                Good p11 k s11 ->
                                                                                                    Good (p1 || p2 || p3 || p4 || p5 || p6 || p7 || p8 || p9 || p10 || p11) (func a b c d e f g h i j k) s11
        )


ignore : Parser x keep -> Parser x ignore -> Parser x keep
ignore keepParser ignoreParser =
    map2 always keepParser ignoreParser


andThen : (a -> Parser x b) -> Parser x a -> Parser x b
andThen callback (Parser parseA) =
    Parser
        (\s0 ->
            case parseA s0 of
                Bad p x ->
                    Bad p x

                Good p1 a s1 ->
                    let
                        (Parser parseB) =
                            callback a
                    in
                    case parseB s1 of
                        Bad p2 x ->
                            Bad (p1 || p2) x

                        Good p2 b s2 ->
                            Good (p1 || p2) b s2
        )


columnAndThen : (Int -> Parser x a) -> Parser x a
columnAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col
            in
            parse s
        )


columnIndentAndThen : (Int -> Int -> Parser x a) -> Parser x a
columnIndentAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.col s.indent
            in
            parse s
        )


offsetSourceAndThen : (Int -> String -> Parser x a) -> Parser x a
offsetSourceAndThen callback =
    Parser
        (\s ->
            let
                (Parser parse) =
                    callback s.offset s.src
            in
            parse s
        )


lazy : (() -> Parser x a) -> Parser x a
lazy thunk =
    Parser
        (\s ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse s
        )


oneOf2 : Parser x a -> Parser x a -> Parser x a
oneOf2 (Parser attemptFirst) (Parser attemptSecond) =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstPStep ->
                    firstPStep

                (Bad p0 firstX) as firstPStep ->
                    if p0 then
                        firstPStep

                    else
                        case attemptSecond s of
                            (Good _ _ _) as secondPStep ->
                                secondPStep

                            (Bad secondCommitted secondX) as secondPStep ->
                                if secondCommitted then
                                    secondPStep

                                else
                                    Bad False (Append (Append Empty firstX) secondX)
        )


orSucceed : Parser x a -> a -> Parser x a
orSucceed (Parser attemptFirst) secondRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstPStep ->
                    firstPStep

                (Bad p0 _) as firstPStep ->
                    if p0 then
                        firstPStep

                    else
                        Good False secondRes s
        )


orSucceedLazy : Parser x a -> (() -> a) -> Parser x a
orSucceedLazy (Parser attemptFirst) createSecondRes =
    Parser
        (\s ->
            case attemptFirst s of
                (Good _ _ _) as firstPStep ->
                    firstPStep

                (Bad p0 _) as firstPStep ->
                    if p0 then
                        firstPStep

                    else
                        Good False (createSecondRes ()) s
        )


oneOf : List (Parser x a) -> Parser x a
oneOf parsers =
    Parser (\s -> oneOfHelp s Empty parsers)


oneOfHelp : State -> Bag x -> List (Parser x a) -> PStep x a
oneOfHelp s0 bag parsers =
    case parsers of
        [] ->
            Bad False bag

        (Parser parse) :: remainingParsers ->
            case parse s0 of
                (Good _ _ _) as step ->
                    step

                (Bad p x) as step ->
                    if p then
                        step

                    else
                        oneOfHelp s0 (Append bag x) remainingParsers


{-| Decide what steps to take next in your [`loop`](#loop).

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
                |> CustomParser.ignore spaces
                |> CustomParser.ignore symbol ";"
                |> CustomParser.ignore spaces
            , succeed ()
                |> map (\_ -> Done (List.reverse revStmts))
            ]

    -- statement : Parser Stmt

Notice that the statements are tracked in reverse as we `Loop`, and we reorder
them only once we are `Done`. This is a very common pattern with `loop`!

Check out [`examples/DoubleQuoteString.elm`](https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm)
for another example.

**IMPORTANT NOTE:** Parsers like `succeed ()` and `chompWhile Char.isAlpha` can
succeed without consuming any characters. So in some cases you may want to use
[`getOffset`](#getOffset) to ensure that each step actually consumed characters.
Otherwise you could end up in an infinite loop!

**Note:** Anything you can write with `loop`, you can also write as a parser
that chomps some characters `andThen` calls itself with new arguments. The
problem with calling `andThen` recursively is that it grows the stack, so you
cannot do it indefinitely. So `loop` is important because enables tail-call
elimination, allowing you to parse however many repeats you want.

-}
loop : state -> (state -> Parser x (Step state a)) -> Parser x a
loop state callback =
    Parser
        (\s -> loopHelp False state callback s)


loopHelp : Bool -> state -> (state -> Parser x (Step state a)) -> State -> PStep x a
loopHelp p state callback s0 =
    let
        (Parser parse) =
            callback state
    in
    case parse s0 of
        Good p1 step s1 ->
            case step of
                Loop newState ->
                    loopHelp (p || p1) newState callback s1

                Done result ->
                    Good (p || p1) result s1

        Bad p1 x ->
            Bad (p || p1) x


backtrackable : Parser x a -> Parser x a
backtrackable (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Bad _ x ->
                    Bad False x

                Good _ a s1 ->
                    Good False a s1
        )


commit : a -> Parser x a
commit a =
    Parser (\s -> Good True a s)


{-| Make sure to never call with String "", as this will then always commit.
-}
keyword : String -> x -> res -> Parser x res
keyword kwd expecting res =
    let
        kwdLength : Int
        kwdLength =
            String.length kwd
    in
    Parser
        (\s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString kwdLength kwd s.offset s.row s.col s.src
            in
            if newOffset == -1 || 0 <= isSubChar (\c -> Char.Extra.isAlphaNumFast c || c == '_') newOffset s.src then
                Bad False (fromState s expecting)

            else
                Good True
                    res
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }
        )


{-| Make sure to never call with String "", as this will then always commit.
-}
keywordFollowedBy : String -> x -> Parser x next -> Parser x next
keywordFollowedBy kwd expecting (Parser parseNext) =
    let
        kwdLength : Int
        kwdLength =
            String.length kwd
    in
    Parser
        (\s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString kwdLength kwd s.offset s.row s.col s.src
            in
            if newOffset == -1 || 0 <= isSubChar (\c -> Char.Extra.isAlphaNumFast c || c == '_') newOffset s.src then
                Bad False (fromState s expecting)

            else
                parseNext
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }
                    |> pStepCommit
        )


{-| Make sure to never call with String "", as this will then always commit.
-}
symbol : String -> x -> res -> Parser x res
symbol str expecting res =
    let
        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString strLength str s.offset s.row s.col s.src
            in
            if newOffset == -1 then
                Bad False (fromState s expecting)

            else
                Good True
                    res
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }
        )


{-| Make sure to never call with String "", as this will then always commit.
-}
symbolFollowedBy : String -> x -> Parser x next -> Parser x next
symbolFollowedBy str expecting (Parser parseNext) =
    let
        strLength : Int
        strLength =
            String.length str
    in
    Parser
        (\s ->
            let
                ( newOffset, newRow, newCol ) =
                    isSubString strLength str s.offset s.row s.col s.src
            in
            if newOffset == -1 then
                Bad False (fromState s expecting)

            else
                parseNext
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = newRow
                    , col = newCol
                    }
                    |> pStepCommit
        )


pStepCommit : PStep x a -> PStep x a
pStepCommit pStep =
    case pStep of
        Good _ a state ->
            Good True a state

        Bad _ errors ->
            Bad True errors


number :
    { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
    -> Parser x a
number c =
    let
        parserAdvancedNumberAndStringLength : Parser.Advanced.Parser c x { length : Int, number : a }
        parserAdvancedNumberAndStringLength =
            Parser.Advanced.map (\n -> \endOffset -> { length = endOffset, number = n })
                (Parser.Advanced.number c)
                |= Parser.Advanced.getOffset
    in
    Parser
        (\state ->
            case Parser.Advanced.run parserAdvancedNumberAndStringLength (String.dropLeft state.offset state.src) of
                Ok result ->
                    Good False result.number (bumpOffset (state.offset + result.length) state)

                Err _ ->
                    Bad False (fromState state c.invalid)
        )


bumpOffset : Int -> State -> State
bumpOffset newOffset s =
    { src = s.src
    , offset = newOffset
    , indent = s.indent
    , row = s.row
    , col = s.col + (newOffset - s.offset)
    }


end : x -> Parser x ()
end x =
    Parser
        (\s ->
            if String.length s.src == s.offset then
                Good False () s

            else
                Bad False (fromState s x)
        )


getChompedString : Parser x a -> Parser x String
getChompedString parser =
    mapChompedString always parser


mapChompedString : (String -> a -> b) -> Parser x a -> Parser x b
mapChompedString func (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Bad p x ->
                    Bad p x

                Good p a s1 ->
                    Good p (func (String.slice s0.offset s1.offset s0.src) a) s1
        )


chompIf : (Char -> Bool) -> x -> Parser x ()
chompIf isGood expecting =
    Parser
        (\s ->
            let
                newOffset : Int
                newOffset =
                    isSubChar isGood s.offset s.src
            in
            if newOffset == -1 then
                -- not found
                Bad False (fromState s expecting)

            else if newOffset == -2 then
                -- newline
                Good True
                    ()
                    { src = s.src
                    , offset = s.offset + 1
                    , indent = s.indent
                    , row = s.row + 1
                    , col = 1
                    }

            else
                -- found
                Good True
                    ()
                    { src = s.src
                    , offset = newOffset
                    , indent = s.indent
                    , row = s.row
                    , col = s.col + 1
                    }
        )


chompWhile : (Char -> Bool) -> Parser x ()
chompWhile isGood =
    Parser
        (\s -> chompWhileHelp isGood s.offset s.row s.col s)


chompWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> State -> PStep x ()
chompWhileHelp isGood offset row col s0 =
    let
        newOffset : Int
        newOffset =
            isSubChar isGood offset s0.src
    in
    if newOffset == -1 then
        -- no match
        Good (s0.offset < offset)
            ()
            { src = s0.src
            , offset = offset
            , indent = s0.indent
            , row = row
            , col = col
            }

    else if newOffset == -2 then
        -- matched a newline
        chompWhileHelp isGood (offset + 1) (row + 1) 1 s0

    else
        -- normal match
        chompWhileHelp isGood newOffset row (col + 1) s0


variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set.Set String
    , expecting : x
    }
    -> Parser x String
variable i =
    Parser
        (\s ->
            let
                firstOffset : Int
                firstOffset =
                    isSubChar i.start s.offset s.src
            in
            if firstOffset == -1 then
                Bad False (fromState s i.expecting)

            else
                let
                    s1 : State
                    s1 =
                        if firstOffset == -2 then
                            varHelp i.inner (s.offset + 1) (s.row + 1) 1 s.src s.indent

                        else
                            varHelp i.inner firstOffset s.row (s.col + 1) s.src s.indent

                    name : String
                    name =
                        String.slice s.offset s1.offset s.src
                in
                if Set.member name i.reserved then
                    Bad False (fromState s i.expecting)

                else
                    Good True name s1
        )


varHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
varHelp isGood offset row col src indent =
    let
        newOffset : Int
        newOffset =
            isSubChar isGood offset src
    in
    if newOffset == -1 then
        { src = src
        , offset = offset
        , indent = indent
        , row = row
        , col = col
        }

    else if newOffset == -2 then
        varHelp isGood (offset + 1) (row + 1) 1 src indent

    else
        varHelp isGood newOffset row (col + 1) src indent


skip : Parser x ignore -> Parser x keep -> Parser x keep
skip iParser kParser =
    map2 revAlways iParser kParser


revAlways : a -> b -> b
revAlways _ b =
    b


nestableMultiComment : String -> x -> String -> x -> Parser x ()
nestableMultiComment oStr oX cStr cX =
    case String.uncons oStr of
        Nothing ->
            problem oX

        Just ( openChar, _ ) ->
            case String.uncons cStr of
                Nothing ->
                    problem cX

                Just ( closeChar, _ ) ->
                    let
                        isNotRelevant : Char -> Bool
                        isNotRelevant char =
                            char /= openChar && char /= closeChar

                        chompOpen : Parser x ()
                        chompOpen =
                            symbol oStr oX ()
                    in
                    ignore chompOpen (nestableHelp isNotRelevant chompOpen (symbol cStr cX ()) cX 1)


nestableHelp : (Char -> Bool) -> Parser x () -> Parser x () -> x -> Int -> Parser x ()
nestableHelp isNotRelevant open close expectingClose nestLevel =
    skip (chompWhile isNotRelevant)
        (oneOf
            [ if nestLevel == 1 then
                close

              else
                close
                    |> andThen (\() -> nestableHelp isNotRelevant open close expectingClose (nestLevel - 1))
            , open
                |> andThen (\() -> nestableHelp isNotRelevant open close expectingClose (nestLevel + 1))
            , ignore (chompIf isNotRelevant expectingClose)
                (chompWhile isNotRelevant)
                |> andThen
                    (\() ->
                        nestableHelp isNotRelevant open close expectingClose nestLevel
                    )
            , chompIf isChar expectingClose
                |> andThen
                    (\() ->
                        nestableHelp isNotRelevant open close expectingClose nestLevel
                    )
            ]
        )


isChar : Char -> Bool
isChar _ =
    True


withIndent : Int -> Parser x a -> Parser x a
withIndent newIndent (Parser parse) =
    Parser
        (\s0 ->
            case parse (changeIndent newIndent s0) of
                Good p a s1 ->
                    Good p a (changeIndent s0.indent s1)

                Bad p x ->
                    Bad p x
        )


changeIndent : Int -> State -> State
changeIndent newIndent s =
    { src = s.src
    , offset = s.offset
    , indent = newIndent
    , row = s.row
    , col = s.col
    }


mapWithStartPosition :
    ({ row : Int, column : Int } -> a -> b)
    -> Parser x a
    -> Parser x b
mapWithStartPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (combineStartAndResult { row = s0.row, column = s0.col } a) s1

                Bad p x ->
                    Bad p x
        )


mapWithEndPosition :
    (a -> { row : Int, column : Int } -> b)
    -> Parser x a
    -> Parser x b
mapWithEndPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (combineStartAndResult a { row = s1.row, column = s1.col }) s1

                Bad p x ->
                    Bad p x
        )


mapWithStartAndEndPosition :
    ({ row : Int, column : Int } -> a -> { row : Int, column : Int } -> b)
    -> Parser x a
    -> Parser x b
mapWithStartAndEndPosition combineStartAndResult (Parser parse) =
    Parser
        (\s0 ->
            case parse s0 of
                Good p a s1 ->
                    Good p (combineStartAndResult { row = s0.row, column = s0.col } a { row = s1.row, column = s1.col }) s1

                Bad p x ->
                    Bad p x
        )



-- LOW-LEVEL HELPERS


{-| When making a fast parser, you want to avoid allocation as much as
possible. That means you never want to mess with the source string, only
keep track of an offset into that string.

You use `isSubString` like this:

    isSubString "let" offset row col "let x = 4 in x"
        --==> ( newOffset, newRow, newCol )

You are looking for `"let"` at a given `offset`. On failure, the
`newOffset` is `-1`. On success, the `newOffset` is the new offset. With
our `"let"` example, it would be `offset + 3`.

You also provide the current `row` and `col` which do not align with
`offset` in a clean way. For example, when you see a `\n` you are at
`row = row + 1` and `col = 1`. Furthermore, some UTF16 characters are
two words wide, so even if there are no newlines, `offset` and `col`
may not be equal.

-}
isSubString : Int -> String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString smallStringLength smallString offset row col bigString =
    -- TODO currently assumes smallString does not contain line \n
    -- TODO currently assumes smallString does not contain UTF-16 characters
    let
        offsetAfter : Int
        offsetAfter =
            offset + smallStringLength
    in
    if String.slice offset offsetAfter bigString == smallString then
        ( offsetAfter, row, col + smallStringLength )

    else
        ( -1, row, col )


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

-}
isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar predicate offset string =
    -- https://github.com/elm/parser/blob/1.1.0/src/Elm/Kernel/Parser.js#L37
    let
        actualChar : String
        actualChar =
            String.slice offset (offset + 1) string
    in
    if charStringIsUtf16HighSurrogate actualChar then
        -- String.all iterates over code points (so here just one Char)
        if String.all predicate (String.slice offset (offset + 2) string) then
            case actualChar of
                "" ->
                    -1

                _ ->
                    offset + 2

        else
            -1

    else if String.all predicate actualChar then
        case actualChar of
            "\n" ->
                -2

            "" ->
                -1

            _ ->
                offset + 1

    else
        -1


charStringIsUtf16HighSurrogate : String -> Bool
charStringIsUtf16HighSurrogate charString =
    charString |> String.all (\c -> c |> Char.toCode |> Basics.toFloat |> Basics.isNaN)
