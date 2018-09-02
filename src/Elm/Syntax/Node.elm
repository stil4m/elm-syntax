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
type Node a
    = Node Range a


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
combine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
combine f ((Node r1 _) as a) ((Node r2 _) as b) =
    Node
        (Range.combine [ r1, r2 ])
        (f a b)


{-| Map the value within a node leaving the range untouched
-}
map : (a -> b) -> Node a -> Node b
map f (Node r a) =
    Node r (f a)


{-| Extract the range out of a `Node a`
-}
range : Node a -> Range
range (Node r _) =
    r


{-| Extract the value (`a`) out of a `Node a`
-}
value : Node a -> a
value (Node _ v) =
    v


{-| Encode a `Node` into JSON
-}
encode : (a -> Value) -> Node a -> Value
encode f (Node r v) =
    JE.object
        [ ( "range", Range.encode r )
        , ( "value", f v )
        ]


{-| A JSON decoder for `Node`
-}
decoder : Decoder a -> Decoder (Node a)
decoder sub =
    JD.map2 Node
        (JD.field "range" Range.decoder)
        (JD.field "value" sub)
