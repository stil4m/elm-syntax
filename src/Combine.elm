module Combine exposing
    ( Parser(..)
    , Step(..)
    , andThen
    , between
    , continueWith
    , end
    , fromCore
    , ignore
    , keep
    , lazy
    , location
    , loop
    , many
    , many1
    , many1Ignore
    , many1WithEndLocationForLastElement
    , manyIgnore
    , manyWithEndLocationForLastElement
    , map
    , maybe
    , maybeIgnore
    , modifyState
    , oneOf
    , parens
    , problem
    , runParser
    , sepBy
    , sepBy1
    , sepBy1Core
    , sepBy1WithoutReverse
    , succeed
    , symbol
    , withLocation
    , withState
    )

import Elm.Syntax.Range exposing (Location, Range)
import Parser as Core exposing ((|.), (|=))


type Parser state res
    = Parser (state -> Core.Parser ( state, res ))


fromCore : Core.Parser res -> Parser state res
fromCore p =
    Parser
        (\state ->
            Core.map (\v -> ( state, v )) p
        )


runParser : Parser state res -> state -> String -> Result (List Core.DeadEnd) ( state, res )
runParser (Parser p) st s =
    Core.run (p st) s


lazy : (() -> Parser state a) -> Parser state a
lazy t =
    Parser
        (\state ->
            Core.lazy
                (\() ->
                    let
                        (Parser t_) =
                            t ()
                    in
                    t_ state
                )
        )


withState : (state -> Parser state a) -> Parser state a
withState f =
    Parser <|
        \state ->
            let
                (Parser p) =
                    f state
            in
            p state


modifyState : (state -> state) -> Parser state ()
modifyState f =
    Parser <|
        \state -> Core.succeed ( f state, () )


withLocation : (Location -> Parser state a) -> Parser state a
withLocation f =
    Parser <|
        \state ->
            Core.getPosition
                |> Core.andThen
                    (\( row, col ) ->
                        let
                            (Parser p) =
                                f { row = row, column = col }
                        in
                        p state
                    )


location : Parser state Location
location =
    Parser <|
        \state ->
            Core.getPosition
                |> Core.andThen
                    (\( row, col ) ->
                        Core.succeed ( state, { row = row, column = col } )
                    )


map : (a -> b) -> Parser state a -> Parser state b
map f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.map (\( s, a ) -> ( s, f a ))


andThen : (a -> Parser state b) -> Parser state a -> Parser state b
andThen f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.andThen
                    (\( s, a ) ->
                        let
                            (Parser x) =
                                f a
                        in
                        x s
                    )


keep : Parser state a -> Parser state (a -> b) -> Parser state b
keep (Parser rp) (Parser lp) =
    Parser <|
        \state ->
            lp state
                |> Core.andThen (\( newState, aToB ) -> Core.map (\( s, a ) -> ( s, aToB a )) (rp newState))


problem : String -> Parser state a
problem m =
    Parser <|
        \_ ->
            Core.problem m


succeed : a -> Parser state a
succeed res =
    Parser <| \state -> Core.succeed ( state, res )


symbol : String -> Parser state ()
symbol str =
    Core.symbol str
        |> fromCore


end : Parser state ()
end =
    Parser <|
        \state ->
            Core.end |> Core.map (\x -> ( state, x ))


oneOf : List (Parser state a) -> Parser state a
oneOf xs =
    Parser <| \state -> Core.oneOf (List.map (\(Parser x) -> x state) xs)


maybe : Parser state a -> Parser state (Maybe a)
maybe (Parser p) =
    Parser <|
        \state ->
            Core.oneOf
                [ p state |> Core.map (\( c, v ) -> ( c, Just v ))
                , Core.succeed ( state, Nothing )
                ]


maybeIgnore : Parser state () -> Parser state ()
maybeIgnore (Parser p) =
    Parser <|
        \state ->
            Core.oneOf
                [ p state
                , Core.succeed ( state, () )
                ]


many : Parser state a -> Parser state (List a)
many p =
    manyWithoutReverse [] p
        |> map List.reverse


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
manyWithoutReverse : List a -> Parser state a -> Parser state (List a)
manyWithoutReverse initList (Parser p) =
    let
        helper : ( state, List a ) -> Core.Parser (Core.Step ( state, List a ) ( state, List a ))
        helper (( oldState, items ) as acc) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed (Core.Done acc)
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, initList ) helper


manyIgnore : Parser state a -> Parser state ()
manyIgnore (Parser p) =
    let
        helper : state -> Core.Parser (Core.Step state ( state, () ))
        helper state =
            Core.oneOf
                [ p state
                    |> Core.map (\( newState, _ ) -> Core.Loop newState)
                , Core.succeed (Core.Done ( state, () ))
                ]
    in
    Parser <|
        \state ->
            Core.loop state helper


manyCore : Core.Parser a -> Core.Parser (List a)
manyCore p =
    manyWithoutReverseCore [] p
        |> Core.map List.reverse


manyWithoutReverseCore : List a -> Core.Parser a -> Core.Parser (List a)
manyWithoutReverseCore initList p =
    let
        helper : List a -> Core.Parser (Core.Step (List a) (List a))
        helper items =
            Core.oneOf
                [ p
                    |> Core.map (\item -> Core.Loop (item :: items))
                , Core.succeed (Core.Done items)
                ]
    in
    Core.loop initList helper


many1Ignore : Parser state a -> Parser state ()
many1Ignore p =
    p
        |> continueWith (manyIgnore p)


manyWithEndLocationForLastElement : Range -> (a -> Range) -> Parser state a -> Parser state ( Location, List a )
manyWithEndLocationForLastElement defaultRange getRange (Parser p) =
    let
        helper : ( state, List a ) -> Core.Parser (Core.Step ( state, List a ) ( state, ( Location, List a ) ))
        helper ( oldState, items ) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed ()
                    |> Core.map
                        (\() ->
                            Core.Done ( oldState, ( endLocationForList defaultRange getRange items, List.reverse items ) )
                        )
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, [] ) helper


many1WithEndLocationForLastElement : (a -> Range) -> Parser state a -> Parser state ( Location, List a )
many1WithEndLocationForLastElement getRange p =
    p
        |> andThen
            (\a ->
                manyWithEndLocationForLastElement (getRange a) getRange p
                    |> map (\( location_, list ) -> ( location_, a :: list ))
            )


endLocationForList : Range -> (a -> Range) -> List a -> Location
endLocationForList defaultRange getRange list =
    case list of
        [] ->
            defaultRange.end

        a :: _ ->
            (getRange a).end


type Step a b
    = Loop a
    | Done b


loop : a -> (a -> Parser state (Step a b)) -> Parser state b
loop init stepper =
    let
        wrapper : ( state, a ) -> Core.Parser (Core.Step ( state, a ) ( state, b ))
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


many1 : Parser state a -> Parser state (List a)
many1 p =
    succeed cons
        |> keep p
        |> keep (many p)


sepBy : Parser state x -> Parser state a -> Parser state (List a)
sepBy sep p =
    oneOf
        [ sepBy1 sep p
        , succeed []
        ]


sepBy1 : Parser state x -> Parser state a -> Parser state (List a)
sepBy1 sep p =
    succeed cons
        |> keep p
        |> keep (many (sep |> continueWith p))


sepBy1Core : Core.Parser x -> Core.Parser a -> Core.Parser (List a)
sepBy1Core sep p =
    Core.succeed cons
        |= p
        |= manyCore
            (Core.succeed identity
                |. sep
                |= p
            )


{-| Same as [`sepBy1`](#sepBy1), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
sepBy1WithoutReverse : Parser state x -> Parser state a -> Parser state (List a)
sepBy1WithoutReverse sep p =
    p
        |> andThen (\first -> manyWithoutReverse [ first ] (sep |> continueWith p))


between : Parser state l -> Parser state r -> Parser state a -> Parser state a
between lp rp p =
    lp
        |> continueWith p
        |> ignore rp


parens : Parser state a -> Parser state a
parens =
    between (symbol "(") (symbol ")")


ignore : Parser state x -> Parser state a -> Parser state a
ignore dropped target =
    target
        |> map always
        |> keep dropped


continueWith : Parser state a -> Parser state x -> Parser state a
continueWith target dropped =
    dropped
        |> map (\_ a -> a)
        |> keep target


cons : a -> List a -> List a
cons first =
    \rest -> first :: rest
