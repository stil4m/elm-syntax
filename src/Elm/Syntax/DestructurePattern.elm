module Elm.Syntax.DestructurePattern exposing
    ( moduleNames
    , encode, decoder
    , DestructurePattern(..)
    )

{-|


# Destructur pattern Syntax

This syntax represents patterns used for destructuring data.
For example:

    Just x as someMaybe
    {name, age}


# Types

@docs DestructurPattern


## Functions

@docs moduleNames


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Custom type for all destructur patterns such as:

  - `AllPattern_`: `_`
  - `UnitPattern_`: `()`
  - `TuplePattern_`: `(a, b)`
  - `RecordPattern_`: `{name, age}`
  - `VarPattern_`: `x`
  - `NamedPattern_`: `Just _`
  - `AsPattern_`: `_ as x`
  - `ParenthesizedPattern_`: `( _ )`

-}
type DestructurePattern r
    = AllPattern_
    | UnitPattern_
    | TuplePattern_ (List (Node r (DestructurePattern r)))
    | RecordPattern_ (List (Node r String))
    | VarPattern_ String
    | NamedPattern_ QualifiedNameRef (List (Node r (DestructurePattern r)))
    | AsPattern_ (Node r (DestructurePattern r)) (Node r String)
    | ParenthesizedPattern_ (Node r (DestructurePattern r))


{-| Get all the modules names that are used in the pattern (and its nested patterns).
Use this to collect qualified patterns, such as `Maybe.Just x`.
-}
moduleNames : DestructurePattern r -> List ModuleName
moduleNames p =
    let
        recur =
            Node.value >> moduleNames
    in
    case p of
        TuplePattern_ xs ->
            List.concatMap recur xs

        RecordPattern_ _ ->
            []

        NamedPattern_ qualifiedNameRef subPatterns ->
            qualifiedNameRef.moduleName :: List.concatMap recur subPatterns

        AsPattern_ inner _ ->
            recur inner

        ParenthesizedPattern_ inner ->
            recur inner

        _ ->
            []



-- Serialization


{-| Encode a `DestructurPattern` syntax element to JSON.
-}
encode : DestructurePattern r -> Value
encode pattern =
    case pattern of
        AllPattern_ ->
            encodeTyped "all" (JE.object [])

        UnitPattern_ ->
            encodeTyped "unit" (JE.object [])

        TuplePattern_ patterns ->
            encodeTyped "tuple"
                (JE.object
                    [ ( "value", JE.list (Node.encode encode) patterns )
                    ]
                )

        RecordPattern_ pointers ->
            encodeTyped "record"
                (JE.object
                    [ ( "value", JE.list (Node.encode JE.string) pointers )
                    ]
                )

        VarPattern_ name ->
            encodeTyped "var"
                (JE.object
                    [ ( "value", JE.string name )
                    ]
                )

        NamedPattern_ qualifiedNameRef patterns ->
            encodeTyped "named" <|
                JE.object
                    [ ( "qualified"
                      , JE.object
                            [ ( "moduleName", ModuleName.encode qualifiedNameRef.moduleName )
                            , ( "name", JE.string qualifiedNameRef.name )
                            ]
                      )
                    , ( "patterns", JE.list (Node.encode encode) patterns )
                    ]

        AsPattern_ destructured name ->
            encodeTyped "as" <|
                JE.object
                    [ ( "name", Node.encode JE.string name )
                    , ( "pattern", Node.encode encode destructured )
                    ]

        ParenthesizedPattern_ p1 ->
            encodeTyped "parentisized"
                (JE.object
                    [ ( "value", Node.encode encode p1 )
                    ]
                )


{-| JSON decoder for a `DestructurPattern` syntax element.
-}
decoder : Decoder (DestructurePattern r)
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "all", JD.succeed AllPattern_ )
                , ( "unit", JD.succeed UnitPattern_ )
                , ( "tuple", JD.field "value" (JD.list (Node.decoder decoder)) |> JD.map TuplePattern_ )
                , ( "record", JD.field "value" (JD.list (Node.decoder JD.string)) |> JD.map RecordPattern_ )
                , ( "var", JD.field "value" JD.string |> JD.map VarPattern_ )
                , ( "named", JD.map2 NamedPattern_ (JD.field "qualified" decodeQualifiedNameRef) (JD.field "patterns" (JD.list (Node.decoder decoder))) )
                , ( "as", JD.map2 AsPattern_ (JD.field "pattern" (Node.decoder decoder)) (JD.field "name" (Node.decoder JD.string)) )
                , ( "parentisized", JD.map ParenthesizedPattern_ (JD.field "value" (Node.decoder decoder)) )
                ]
        )


decodeQualifiedNameRef : Decoder QualifiedNameRef
decodeQualifiedNameRef =
    JD.map2 QualifiedNameRef
        (JD.field "moduleName" ModuleName.decoder)
        (JD.field "name" JD.string)
