module Combine exposing
    ( Parser(..)
    , Step(..)
    , andThen
    , andThenFromCore
    , backtrackable
    , between
    , continueFromCore
    , continueWith
    , continueWithCore
    , continueWithFromCore
    , end
    , fromCore
    , fromCoreIgnore
    , fromCoreKeep
    , fromCoreMap
    , ignore
    , ignoreEntirely
    , keep
    , keepFromCore
    , lazy
    , loop
    , many
    , many1
    , many1Ignore
    , manyIgnore
    , manyWithEndLocationForLastElement
    , map
    , maybeIgnore
    , modifyState
    , oneOf
    , parens
    , problem
    , runParser
    , sepBy
    , sepBy1
    , sepBy1WithState
    , sepBy1WithoutReverse
    , succeed
    , succeedLazy
    , withColumn
    , withState
    , withStateFromCore
    )

import Elm.Syntax.Range exposing (Location, Range)
import Parser as Core exposing ((|.), (|=))


type Parser state res
    = Parser (state -> Core.Parser ( state, res ))


fromCoreMap : (res -> changedRes) -> Core.Parser res -> Parser state changedRes
fromCoreMap resChange p =
    Parser
        (\state ->
            Core.map (\v -> ( state, resChange v )) p
        )


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
            let
                (Parser t_) =
                    t ()
            in
            t_ state
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


withStateFromCore : (state -> Core.Parser ( state, a )) -> Parser state a
withStateFromCore f =
    Parser <|
        \state -> f state


modifyState : (state -> state) -> Parser state ()
modifyState f =
    Parser <|
        \state -> Core.succeed ( f state, () )


withColumn : (Int -> Parser state a) -> Parser state a
withColumn f =
    Parser <|
        \state ->
            Core.getCol
                |> Core.andThen
                    (\col ->
                        let
                            (Parser p) =
                                f col
                        in
                        p state
                    )


map : (a -> b) -> Parser state a -> Parser state b
map f (Parser p) =
    Parser <|
        \state ->
            p state
                |> Core.map (\( s, a ) -> ( s, f a ))


andThenFromCore : (a -> Parser state b) -> Core.Parser a -> Parser state b
andThenFromCore f p =
    Parser <|
        \state ->
            p
                |> Core.andThen
                    (\a ->
                        let
                            (Parser x) =
                                f a
                        in
                        x state
                    )


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


fromCoreKeep : Parser state a -> Core.Parser (a -> b) -> Parser state b
fromCoreKeep (Parser rp) lp =
    Parser <|
        \state ->
            lp
                |> Core.andThen
                    (\aToB ->
                        rp state
                            |> Core.map (\( newState, a ) -> ( newState, aToB a ))
                    )


keepFromCore : Core.Parser a -> Parser state (a -> b) -> Parser state b
keepFromCore rp (Parser lp) =
    Parser <|
        \state ->
            lp state
                |> Core.andThen (\( newState, aToB ) -> Core.map (\a -> ( newState, aToB a )) rp)


problem : String -> Parser state a
problem m =
    Parser <|
        \_ ->
            Core.problem m


succeed : a -> Parser state a
succeed res =
    Parser <| \state -> Core.succeed ( state, res )


succeedLazy : (() -> a) -> Parser state a
succeedLazy createRes =
    Parser
        (\state ->
            Core.succeed ( state, createRes () )
        )


end : Parser state ()
end =
    Parser <|
        \state ->
            Core.end |> Core.map (\x -> ( state, x ))


oneOf : List (Parser state a) -> Parser state a
oneOf xs =
    Parser <| \state -> Core.oneOf (List.map (\(Parser x) -> x state) xs)


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
        manyWithoutReverseStep : ( state, List a ) -> Core.Parser (Core.Step ( state, List a ) ( state, List a ))
        manyWithoutReverseStep (( oldState, items ) as acc) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.succeed (Core.Done acc)
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, initList ) manyWithoutReverseStep


manyIgnore : Parser state () -> Parser state ()
manyIgnore (Parser p) =
    let
        helper : state -> Core.Parser (Core.Step state ( state, () ))
        helper state =
            Core.oneOf
                [ p state
                    |> Core.map (\( newState, () ) -> Core.Loop newState)
                , Core.succeed (Core.Done ( state, () ))
                ]
    in
    Parser <|
        \state ->
            Core.loop state helper


many1Ignore : Parser state () -> Parser state ()
many1Ignore p =
    p
        |> continueWith (manyIgnore p)


{-| Attention: This will return `{ row = 0, column = 0 }` as the end location if the resulting list is empty!
-}
manyWithEndLocationForLastElement : (a -> Range) -> Parser state a -> Parser state ( Location, List a )
manyWithEndLocationForLastElement getRange (Parser p) =
    let
        helper : ( state, List a ) -> Core.Parser (Core.Step ( state, List a ) ( state, ( Location, List a ) ))
        helper ( oldState, items ) =
            Core.oneOf
                [ p oldState
                    |> Core.map (\( newState, item ) -> Core.Loop ( newState, item :: items ))
                , Core.lazy
                    (\() ->
                        Core.succeed
                            (Core.Done ( oldState, ( endLocationForList getRange items, List.reverse items ) ))
                    )
                ]
    in
    Parser <|
        \state ->
            Core.loop ( state, [] ) helper


endLocationForList : (a -> Range) -> List a -> Location
endLocationForList getRange list =
    case list of
        [] ->
            emptyLocation

        a :: _ ->
            (getRange a).end


emptyLocation : Location
emptyLocation =
    { row = 0, column = 0 }


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


sepBy : String -> Parser state a -> Parser state (List a)
sepBy sep p =
    oneOf
        [ sepBy1 sep p
        , succeed []
        ]


sepBy1 : String -> Parser state a -> Parser state (List a)
sepBy1 sep p =
    map cons p
        |> keep (many (Core.symbol sep |> continueWithFromCore p))


sepBy1WithState : Parser state () -> Parser state a -> Parser state (List a)
sepBy1WithState sep p =
    map cons p
        |> keep (many (sep |> continueWith p))


{-| Same as [`sepBy1`](#sepBy1), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.
-}
sepBy1WithoutReverse : Parser state () -> Parser state a -> Parser state (List a)
sepBy1WithoutReverse sep p =
    p
        |> andThen (\first -> manyWithoutReverse [ first ] (sep |> continueWith p))


between : String -> String -> Parser state a -> Parser state a
between lp rp (Parser p) =
    Parser <|
        \state ->
            Core.map (\() -> identity) (Core.symbol lp)
                |= p state
                |. Core.symbol rp


parens : Parser state a -> Parser state a
parens p =
    between "(" ")" p


ignore : Parser state () -> Parser state a -> Parser state a
ignore (Parser dropped) (Parser target) =
    Parser <|
        \state ->
            target state
                |> Core.andThen
                    (\( newState, a ) ->
                        dropped newState
                            |> Core.map (\( finalState, () ) -> ( finalState, a ))
                    )


fromCoreIgnore : Parser state () -> Core.Parser a -> Parser state a
fromCoreIgnore (Parser dropped) target =
    Parser <|
        \state ->
            target
                |> Core.andThen
                    (\a ->
                        dropped state
                            |> Core.map (\( newState, () ) -> ( newState, a ))
                    )


ignoreEntirely : Core.Parser () -> Parser state a -> Parser state a
ignoreEntirely dropped (Parser target) =
    Parser <|
        \state ->
            target state
                |. dropped


continueWith : Parser state a -> Parser state () -> Parser state a
continueWith target dropped =
    dropped
        |> andThen (\() -> target)


backtrackable : Parser state a -> Parser state a
backtrackable (Parser p) =
    Parser (\state -> p state |> Core.backtrackable)


continueWithFromCore : Parser state a -> Core.Parser () -> Parser state a
continueWithFromCore target dropped =
    dropped
        |> andThenFromCore (\() -> target)


continueWithCore : Core.Parser a -> Parser state () -> Parser state a
continueWithCore target (Parser dropped) =
    Parser <|
        \state ->
            dropped state
                |> Core.andThen
                    (\( newState, () ) ->
                        target
                            |> Core.map (\a -> ( newState, a ))
                    )


continueFromCore : Parser state a -> Core.Parser () -> Parser state a
continueFromCore (Parser target) dropped =
    Parser <|
        \state ->
            dropped
                |> Core.andThen
                    (\() ->
                        target state
                    )


cons : a -> List a -> List a
cons first =
    \rest -> first :: rest
