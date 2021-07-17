module Elm.Syntax.Node exposing
    ( Node(..)
    , combine, range, value, map
    , encode, decoder
    )

{-| Abstract Syntax Nodes


# Types

@docs Node


# Functions

@docs combine, range, value, map


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Range as Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Base representation for a syntax node in a source file.
-}
type Node r a
    = Node r a


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
combine : (Node Range a -> Node Range b -> c) -> Node Range a -> Node Range b -> Node Range c
combine f ((Node r1 _) as a) ((Node r2 _) as b) =
    Node
        (Range.combine [ r1, r2 ])
        (f a b)


{-| Map the value within a node leaving the range untouched
-}
map : (a -> b) -> Node r a -> Node r b
map f (Node r a) =
    Node r (f a)


{-| Extract the range out of a `Node Range a`
-}
range : Node r a -> r
range (Node r _) =
    r


{-| Extract the value (`a`) out of a `Node Range a`
-}
value : Node r a -> a
value (Node _ v) =
    v


{-| Encode a `Node` into JSON
-}
encode : (a -> Value) -> Node r a -> Value
encode f (Node r v) =
    JE.object
        [ ( "range", Range.encode r )
        , ( "value", f v )
        ]


{-| A JSON decoder for `Node`
-}
decoder : Decoder a -> Decoder (Node r a)
decoder sub =
    JD.map2 Node
        (JD.field "range" Range.decoder)
        (JD.field "value" sub)
