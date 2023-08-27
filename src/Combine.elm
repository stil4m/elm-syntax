module Combine exposing
    ( ParseFn
    , Parser(..)
    , Step(..)
    , andMap
    , andThen
    , backtrackable
    , between
    , continueWith
    , end
    , fail
    , fromCore
    , ignore
    , lazy
    , loop
    , many
    , many1
    , map
    , maybe
    , modifyState
    , oneOf
    , or
    , parens
    , runParser
    , sepBy
    , sepBy1
    , string
    , succeed
    , while
    , withLocation
    , withState
    )

import Elm.Syntax.Range exposing (Location)
import Parser as Core exposing ((|=))


type alias ParseFn state res =
    state -> Core.Parser ( state, res )


type Parser state res
    = Parser (state -> Core.Parser ( state, res ))


fromCore : Core.Parser res -> Parser state res
fromCore p =
    Parser
        (\state ->
            Core.succeed (\v -> ( state, v )) |= p
        )


app : Parser state res -> ParseFn state res
app (Parser inner) =
    inner


runParser : Parser state res -> state -> String -> Result (List Core.DeadEnd) ( state, res )
runParser (Parser p) st s =
    Core.run (p st) s


lazy : (() -> Parser s a) -> Parser s a
lazy t =
    Parser (\state -> Core.lazy (\() -> (\(Parser t_) -> t_ state) (t ())))


withState : (s -> Parser s a) -> Parser s a
withState f =
    Parser <|
        \state ->
            (\(Parser p) -> p state) (f state)


modifyState : (s -> s) -> Parser s ()
modifyState f =
    Parser <|
        \state -> Core.succeed ( f state, () )


withLocation : (Location -> Parser s a) -> Parser s a
withLocation f =
    Parser <|
        \state ->
            Core.getPosition
                |> Core.andThen (\( row, col ) -> app (f { row = row, column = col }) state)


map : (a -> b) -> Parser s a -> Parser s b
map f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.map (\( s, a ) -> ( s, f a ))


andThen : (a -> Parser s b) -> Parser s a -> Parser s b
andThen f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.andThen (\( s, a ) -> (\(Parser x) -> x s) (f a))


andMap : Parser s a -> Parser s (a -> b) -> Parser s b
andMap (Parser rp) (Parser lp) =
    Parser <|
        \state ->
            lp state
                |> Core.andThen (\( newState, a ) -> Core.map (Tuple.mapSecond a) (rp newState))


fail : String -> Parser s a
fail m =
    Parser <|
        \state ->
            Core.problem m |> Core.map (\x -> ( state, x ))


succeed : a -> Parser s a
succeed res =
    Parser <| \state -> Core.succeed ( state, res )


string : String -> Parser s String
string s =
    Parser <|
        \state ->
            Core.getChompedString (Core.token s)
                |> Core.map (\x -> ( state, x ))


while : (Char -> Bool) -> Parser s String
while pred =
    Parser <|
        \state ->
            Core.getChompedString (Core.chompWhile pred)
                |> Core.map (\x -> ( state, x ))


end : Parser s ()
end =
    Parser <|
        \state ->
            Core.end |> Core.map (\x -> ( state, x ))


or : Parser s a -> Parser s a -> Parser s a
or (Parser lp) (Parser rp) =
    Parser <|
        \state ->
            Core.oneOf
                [ lp state
                , rp state
                ]


backtrackable : Parser s a -> Parser s a
backtrackable (Parser p) =
    Parser <| \state -> Core.backtrackable (p state)


oneOf : List (Parser s a) -> Parser s a
oneOf xs =
    Parser <| \state -> Core.oneOf (List.map (\(Parser x) -> x state) xs)


maybe : Parser s a -> Parser s (Maybe a)
maybe (Parser p) =
    Parser <|
        \state ->
            Core.oneOf
                [ p state |> Core.map (\( c, v ) -> ( c, Just v ))
                , Core.succeed ( state, Nothing )
                ]


many : Parser s a -> Parser s (List a)
many p =
    let
        helper : ( s, List a ) -> Core.Parser (Core.Step ( s, List a ) ( s, List a ))
        helper ( oldState, items ) =
            Core.oneOf
                [ Core.succeed (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                    |= app p oldState
                , Core.succeed ()
                    |> Core.map (\_ -> Core.Done ( oldState, List.reverse items ))
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, [] ) helper


type Step a b
    = Loop a
    | Done b


loop : a -> (a -> Parser s (Step a b)) -> Parser s b
loop init stepper =
    let
        wrapper ( oldState, v ) =
            let
                (Parser p) =
                    stepper v
            in
            p oldState
                |> Core.map
                    (\( newState, r ) ->
                        case r of
                            Loop l ->
                                Core.Loop ( newState, l )

                            Done d ->
                                Core.Done ( newState, d )
                    )
    in
    Parser <| \state -> Core.loop ( state, init ) wrapper


many1 : Parser s a -> Parser s (List a)
many1 p =
    succeed (::)
        |> andMap p
        |> andMap (many p)


sepBy : Parser s x -> Parser s a -> Parser s (List a)
sepBy sep p =
    or (sepBy1 sep p) (succeed [])


sepBy1 : Parser s x -> Parser s a -> Parser s (List a)
sepBy1 sep p =
    succeed (::)
        |> andMap p
        |> andMap (many (sep |> continueWith p))


between : Parser s l -> Parser s r -> Parser s a -> Parser s a
between lp rp p =
    lp
        |> continueWith p
        |> ignore rp


parens : Parser s a -> Parser s a
parens =
    between (string "(") (string ")")


ignore : Parser s x -> Parser s a -> Parser s a
ignore dropped target =
    target
        |> map always
        |> andMap dropped


continueWith : Parser s a -> Parser s x -> Parser s a
continueWith target dropped =
    dropped
        |> map (\_ a -> a)
        |> andMap target
