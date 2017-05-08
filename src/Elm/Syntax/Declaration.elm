module Elm.Syntax.Declaration
    exposing
        ( Declaration
            ( FuncDecl
            , AliasDecl
            , PortDeclaration
            , TypeDecl
            , InfixDeclaration
            , Destructuring
            )
        )

{-| Declarations Syntax


# Types

@docs Declaration

-}

import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Expression exposing (Expression, FunctionSignature, Function)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.Type exposing (Type)


{-| Union type that represents all different top level declarations.
-}
type Declaration
    = FuncDecl Function
    | AliasDecl TypeAlias
    | TypeDecl Type
    | PortDeclaration FunctionSignature
    | InfixDeclaration Infix
    | Destructuring Pattern Expression
