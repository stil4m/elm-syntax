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
            , RecordPattern
            , StringPattern
            , TuplePattern
            , UnConsPattern
            , UnitPattern
            , VarPattern
            )
        , QualifiedNameRef
        , moduleNames
        )

{-| Pattern Syntax


# Types

@docs Pattern, QualifiedNameRef


# Functions

@docs moduleNames

-}

import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)


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
    | AsPattern (Ranged Pattern) VariablePointer
    | ParenthesizedPattern (Ranged Pattern)


{-| Qualified name reference
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }


{-| Get all the modules names that are used in the pattern match. Use this to collect qualified patterns, such as `Maybe.Just x`.
-}
moduleNames : Pattern -> List ModuleName
moduleNames p =
    let
        recur =
            Ranged.value >> moduleNames
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
