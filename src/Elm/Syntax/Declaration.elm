module Elm.Syntax.Declaration exposing (Declaration(..))

{-| Syntax for the different top-level declarations in Elm.
These can be one of the following (all declared in `Declaration`):

  - Functions: `add x y = x + y`
  - Custom types: `type Color = Blue | Red`
  - Type aliases: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declarations. You will probably not need this, while only core packages can define these.


## Types

@docs Declaration

-}

import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)


{-| Custom type that represents all different top-level declarations.
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration { documentation : Maybe (Node Documentation), signature : Node Signature }
    | InfixDeclaration Infix
