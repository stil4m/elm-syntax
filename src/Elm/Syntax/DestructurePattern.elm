module Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))

{-|


# Destructure pattern Syntax

This syntax represents patterns used for destructuring data.
For example:

    Just x as someMaybe
    {name, age}

@docs DestructurePattern

-}

import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)


{-| Custom type for all destructuring patterns such as:

  - `AllPattern_`: `_`
  - `UnitPattern_`: `()`
  - `TuplePattern_`: `(a, b)`
  - `RecordPattern_`: `{name, age}`
  - `VarPattern_`: `x`
  - `NamedPattern_`: `Just _`
  - `AsPattern_`: `_ as x`
  - `ParenthesizedPattern_`: `( _ )`

-}
type DestructurePattern
    = AllPattern_
    | UnitPattern_
    | TuplePattern_ (List (Node DestructurePattern))
    | RecordPattern_ (List (Node String))
    | VarPattern_ String
    | NamedPattern_ QualifiedNameRef (List (Node DestructurePattern))
    | AsPattern_ (Node DestructurePattern) (Node String)
    | ParenthesizedPattern_ (Node DestructurePattern)
