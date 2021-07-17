module Elm.Syntax.Exposing exposing
    ( Exposing(..), TopLevelExpose(..), ExposedType
    , exposesFunction, operators
    , encode, decoder
    )

{-|


# Exposing Syntax

This syntax represents the exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)


# Types

@docs Exposing, TopLevelExpose, ExposedType


# Functions

@docs exposesFunction, operators


# Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Diffent kind of exposing declarations
-}
type Exposing r
    = All r
    | Explicit (Node r (TopLevelExpose r)) (List (Node r (TopLevelExpose r)))


{-| An exposed entity
-}
type TopLevelExpose r
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose (ExposedType r)


{-| Exposed Type
-}
type alias ExposedType r =
    { name : Node r String
    , open : Maybe r
    }



-- Functions


{-| Check whether an import/module exposing list exposes a certain function. Will yield `True` if `Exposing` is exposing everything (`All`).

    exposesFunction "something" (All someRange) == True

    exposesFunction "divide" (Explicit [ Node someRange (FunctionExpose "add") ]) == False

    exposesFunction "add" (Explicit [ Node someRange (FunctionExpose "add") ]) == True

-}
exposesFunction : String -> Exposing r -> Bool
exposesFunction s exposure =
    case exposure of
        All _ ->
            True

        Explicit head rest ->
            List.any
                (\(Node _ value) ->
                    case value of
                        FunctionExpose fun ->
                            fun == s

                        _ ->
                            False
                )
                (head :: rest)


{-| Collect all operator names from a list of TopLevelExposes
-}
operators : List (TopLevelExpose r) -> List String
operators l =
    List.filterMap operator l


operator : TopLevelExpose r -> Maybe String
operator t =
    case t of
        InfixExpose s ->
            Just s

        _ ->
            Nothing



-- Serialization


{-| Encode an `Exposing` syntax element to JSON.
-}
encode : Exposing r -> Value
encode exp =
    case exp of
        All r ->
            encodeTyped "all" <| Range.encode r

        Explicit head rest ->
            encodeTyped "explicit" (JE.list encodeTopLevelExpose (head :: rest))


{-| JSON decoder for an `Exposing` syntax element.
-}
decoder : Decoder (Exposing r)
decoder =
    decodeTyped
        [ ( "all", Range.decoder |> JD.map All )
        , ( "explicit", decodeNonemptyList topLevelExposeDecoder |> JD.map (\( head, rest ) -> Explicit head rest) )
        ]


decodeNonemptyList : Decoder a -> Decoder ( a, List a )
decodeNonemptyList decodeA =
    JD.list decodeA
        |> JD.andThen
            (\list ->
                case list of
                    head :: rest ->
                        JD.succeed ( head, rest )

                    [] ->
                        JD.fail "List must have at least one element."
            )


encodeTopLevelExpose : Node Range TopLevelExpose -> Value
encodeTopLevelExpose =
    Node.encode
        (\exp ->
            case exp of
                InfixExpose x ->
                    encodeTyped "infix" <|
                        JE.object
                            [ ( "name", JE.string x )
                            ]

                FunctionExpose x ->
                    encodeTyped "function" <|
                        JE.object
                            [ ( "name", JE.string x )
                            ]

                TypeOrAliasExpose x ->
                    encodeTyped "typeOrAlias" <|
                        JE.object
                            [ ( "name", JE.string x )
                            ]

                TypeExpose exposedType ->
                    encodeTyped "typeexpose" (encodeExposedType exposedType)
        )


encodeExposedType : ExposedType r -> Value
encodeExposedType { name, open } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "open", open |> Maybe.map Range.encode |> Maybe.withDefault JE.null )
        ]


topLevelExposeDecoder : Decoder (Node Range (TopLevelExpose r))
topLevelExposeDecoder =
    Node.decoder
        (decodeTyped
            [ ( "infix", JD.map InfixExpose (JD.field "name" JD.string) )
            , ( "function", JD.map FunctionExpose (JD.field "name" JD.string) )
            , ( "typeOrAlias", JD.map TypeOrAliasExpose (JD.field "name" JD.string) )
            , ( "typeexpose", JD.map TypeExpose exposedTypeDecoder )
            ]
        )


exposedTypeDecoder : Decoder (ExposedType r)
exposedTypeDecoder =
    JD.map2 ExposedType
        (JD.field "name" (Node.decoder JD.string))
        (JD.field "open" (JD.nullable Range.decoder))
