module Elm.Syntax.Exposing
    exposing
        ( ExposedType
        , Exposing(All, Explicit, None)
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


{-| Diffent kind of exposing declarations
-}
type Exposing a
    = None
    | All Range
    | Explicit (List a)


{-| An exposed entity
-}
type TopLevelExpose
    = InfixExpose String Range
    | FunctionExpose String Range
    | TypeOrAliasExpose String Range
    | TypeExpose ExposedType


{-| Exposed Type
-}
type alias ExposedType =
    { name : String
    , constructors : Exposing ValueConstructorExpose
    , range : Range
    }


{-| Exposed Value Constructor
-}
type alias ValueConstructorExpose =
    ( String, Range )


{-| Find out the range of a top level expose
-}
topLevelExposeRange : TopLevelExpose -> Range
topLevelExposeRange e =
    case e of
        InfixExpose _ r ->
            r

        FunctionExpose _ r ->
            r

        TypeOrAliasExpose _ r ->
            r

        TypeExpose typeExpose ->
            typeExpose.range


{-| Check whether an import/module exposing list exposes a certain function
-}
exposesFunction : String -> Exposing TopLevelExpose -> Bool
exposesFunction s exposure =
    case exposure of
        All _ ->
            True

        None ->
            False

        Explicit l ->
            List.any
                (\x ->
                    case x of
                        FunctionExpose fun _ ->
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
        InfixExpose s _ ->
            Just s

        _ ->
            Nothing
