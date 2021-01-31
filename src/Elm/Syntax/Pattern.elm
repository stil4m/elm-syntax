module Elm.Syntax.Pattern exposing
    ( Pattern(..), QualifiedNameRef
    , moduleNames
    , encode, decoder
    )

{-|


# Pattern Syntax

This syntax represents the patterns.
For example:

    Just x as someMaybe
    {name, age}


# Types

@docs Pattern, QualifiedNameRef


## Functions

@docs moduleNames


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Custom type for all patterns such as:

  - `AllPattern`: `_`
  - `UnitPattern`: `()`
  - `CharPattern`: `'c'`
  - `StringPattern`: `"hello"`
  - `IntPattern`: `42`
  - `HexPattern`: `0x11`
  - `FloatPattern`: `42.0`
  - `TuplePattern`: `(a, b)`
  - `RecordPattern`: `{name, age}`
  - `UnConsPattern`: `x :: xs`
  - `ListPattern`: `[ x, y ]`
  - `VarPattern`: `x`
  - `NamedPattern`: `Just _`
  - `AsPattern`: `_ as x`
  - `ParenthesizedPattern`: `( _ )`

-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List (Node Pattern))
    | RecordPattern (List (Node String))
    | UnConsPattern (Node Pattern) (Node Pattern)
    | ListPattern (List (Node Pattern))
    | VarPattern String
    | NamedPattern QualifiedNameRef (List (Node Pattern))
    | AsPattern (Node Pattern) (Node String)
    | ParenthesizedPattern (Node Pattern)


{-| Qualified name reference such as `Maybe.Just`.
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }


{-| Get all the modules names that are used in the pattern (and its nested patterns).
Use this to collect qualified patterns, such as `Maybe.Just x`.
-}
moduleNames : Pattern -> List ModuleName
moduleNames p =
    let
        recur =
            Node.value >> moduleNames
    in
    case p of
        TuplePattern xs ->
            List.concatMap recur xs

        RecordPattern _ ->
            []

        UnConsPattern left right ->
            recur left ++ recur right

        ListPattern xs ->
            List.concatMap recur xs

        NamedPattern qualifiedNameRef subPatterns ->
            qualifiedNameRef.moduleName :: List.concatMap recur subPatterns

        AsPattern inner _ ->
            recur inner

        ParenthesizedPattern inner ->
            recur inner

        _ ->
            []



-- Serialization


{-| Encode a `Pattern` syntax element to JSON.
-}
encode : Pattern -> Value
encode pattern =
    case pattern of
        AllPattern ->
            encodeTyped "all" (JE.object [])

        UnitPattern ->
            encodeTyped "unit" (JE.object [])

        CharPattern c ->
            encodeTyped "char"
                (JE.object
                    [ ( "value", JE.string <| String.fromChar c )
                    ]
                )

        StringPattern v ->
            encodeTyped "string"
                (JE.object
                    [ ( "value", JE.string v )
                    ]
                )

        HexPattern h ->
            encodeTyped "hex"
                (JE.object
                    [ ( "value", JE.int h )
                    ]
                )

        IntPattern i ->
            encodeTyped "int"
                (JE.object
                    [ ( "value", JE.int i )
                    ]
                )

        FloatPattern f ->
            encodeTyped "float"
                (JE.object
                    [ ( "value", JE.float f )
                    ]
                )

        TuplePattern patterns ->
            encodeTyped "tuple"
                (JE.object
                    [ ( "value", JE.list (Node.encode encode) patterns )
                    ]
                )

        RecordPattern pointers ->
            encodeTyped "record"
                (JE.object
                    [ ( "value", JE.list (Node.encode JE.string) pointers )
                    ]
                )

        UnConsPattern p1 p2 ->
            encodeTyped "uncons"
                (JE.object
                    [ ( "left", Node.encode encode p1 )
                    , ( "right", Node.encode encode p2 )
                    ]
                )

        ListPattern patterns ->
            encodeTyped "list"
                (JE.object
                    [ ( "value", JE.list (Node.encode encode) patterns )
                    ]
                )

        VarPattern name ->
            encodeTyped "var"
                (JE.object
                    [ ( "value", JE.string name )
                    ]
                )

        NamedPattern qualifiedNameRef patterns ->
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

        AsPattern destructured name ->
            encodeTyped "as" <|
                JE.object
                    [ ( "name", Node.encode JE.string name )
                    , ( "pattern", Node.encode encode destructured )
                    ]

        ParenthesizedPattern p1 ->
            encodeTyped "parentisized"
                (JE.object
                    [ ( "value", Node.encode encode p1 )
                    ]
                )


{-| JSON decoder for a `Pattern` syntax element.
-}
decoder : Decoder Pattern
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "all", JD.succeed AllPattern )
                , ( "unit", JD.succeed UnitPattern )
                , ( "char", JD.field "value" decodeChar |> JD.map CharPattern )
                , ( "string", JD.field "value" JD.string |> JD.map StringPattern )
                , ( "hex", JD.int |> JD.map HexPattern )
                , ( "int", JD.field "value" JD.int |> JD.map IntPattern )
                , ( "float", JD.field "value" JD.float |> JD.map FloatPattern )
                , ( "tuple", JD.field "value" (JD.list (Node.decoder decoder)) |> JD.map TuplePattern )
                , ( "record", JD.field "value" (JD.list (Node.decoder JD.string)) |> JD.map RecordPattern )
                , ( "uncons", JD.map2 UnConsPattern (JD.field "left" (Node.decoder decoder)) (JD.field "right" (Node.decoder decoder)) )
                , ( "list", JD.field "value" (JD.list (Node.decoder decoder)) |> JD.map ListPattern )
                , ( "var", JD.field "value" JD.string |> JD.map VarPattern )
                , ( "named", JD.map2 NamedPattern (JD.field "qualified" decodeQualifiedNameRef) (JD.field "patterns" (JD.list (Node.decoder decoder))) )
                , ( "as", JD.map2 AsPattern (JD.field "pattern" (Node.decoder decoder)) (JD.field "name" (Node.decoder JD.string)) )
                , ( "parentisized", JD.map ParenthesizedPattern (JD.field "value" (Node.decoder decoder)) )
                ]
        )


decodeQualifiedNameRef : Decoder QualifiedNameRef
decodeQualifiedNameRef =
    JD.map2 QualifiedNameRef
        (JD.field "moduleName" ModuleName.decoder)
        (JD.field "name" JD.string)


decodeChar : Decoder Char
decodeChar =
    JD.string
        |> JD.andThen
            (\s ->
                case String.uncons s of
                    Just ( c, _ ) ->
                        JD.succeed c

                    Nothing ->
                        JD.fail "Not a char"
            )
