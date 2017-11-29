module Elm.Syntax.Exposing
    exposing
        ( ExposedType
        , Exposing(All, Explicit)
        , TopLevelExpose(FunctionExpose, InfixExpose, TypeExpose, TypeOrAliasExpose)
        , ValueConstructorExpose
        , exposesFunction
        , operators
        , topLevelExposeRange
        )

{-| Exposing Syntax


# Types

@docs Exposing, TopLevelExpose, ExposedType, ValueConstructorExpose


# Functions

@docs topLevelExposeRange, exposesFunction, operators

-}

import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)


{-| Diffent kind of exposing declarations
-}
type Exposing a
    = All Range
    | Explicit (List a)


{-| An exposed entity
-}
type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


{-| Exposed Type
-}
type alias ExposedType =
    { name : String
    , constructors : Maybe (Exposing ValueConstructorExpose)
    }


{-| Exposed Value Constructor
-}
type alias ValueConstructorExpose =
    Ranged String


{-| Find out the range of a top level expose
-}
topLevelExposeRange : Ranged TopLevelExpose -> Range
topLevelExposeRange ( r, _ ) =
    r


{-| Check whether an import/module exposing list exposes a certain function
-}
exposesFunction : String -> Exposing TopLevelExpose -> Bool
exposesFunction s exposure =
    case exposure of
        All _ ->
            True

        Explicit l ->
            List.any
                (\x ->
                    case x of
                        FunctionExpose fun ->
                            fun == s

                        _ ->
                            False
                )
                l


{-| Get all operator names from a list of TopLevelExposes
-}
operators : List TopLevelExpose -> List String
operators l =
    List.filterMap operator l


operator : TopLevelExpose -> Maybe String
operator t =
    case t of
        InfixExpose s ->
            Just s

        _ ->
            Nothing
