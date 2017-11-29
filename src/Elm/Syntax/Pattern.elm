module Elm.Syntax.Pattern
    exposing
        ( Pattern
            ( AllPattern
            , AsPattern
            , CharPattern
            , FloatPattern
            , IntPattern
            , ListPattern
            , NamedPattern
            , ParenthesizedPattern
            , QualifiedNamePattern
            , RecordPattern
            , StringPattern
            , TuplePattern
            , UnConsPattern
            , UnitPattern
            , VarPattern
            )
        , QualifiedNameRef
        )

{-| Pattern Syntax


# Types

@docs Pattern, QualifiedNameRef

-}

import Elm.Syntax.Base exposing (VariablePointer)
import Elm.Syntax.Ranged exposing (Ranged)


{-| Union type for all the patterns
-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | FloatPattern Float
    | TuplePattern (List (Ranged Pattern))
    | RecordPattern (List VariablePointer)
    | UnConsPattern (Ranged Pattern) (Ranged Pattern)
    | ListPattern (List (Ranged Pattern))
    | VarPattern String
    | NamedPattern QualifiedNameRef (List (Ranged Pattern))
    | QualifiedNamePattern QualifiedNameRef
    | AsPattern (Ranged Pattern) VariablePointer
    | ParenthesizedPattern (Ranged Pattern)


{-| Qualified name reference
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }
