module Elm.Syntax.Declaration exposing (Declaration(..))

{-| Declarations Syntax


# Types

@docs Declaration

-}

import Elm.Syntax.Expression exposing (Expression, Function, FunctionSignature)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


{-| Union type that represents all different top level declarations.
-}
type Declaration
    = FuncDecl Function
    | AliasDecl TypeAlias
    | TypeDecl Type
    | PortDeclaration FunctionSignature
    | InfixDeclaration Infix
    | Destructuring (Ranged Pattern) (Ranged Expression)
