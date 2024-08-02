module Elm.Syntax.Import exposing (Import)

{-| This syntax represents imports in Elm.
For example:

    import Html.Attributes as HA exposing (style)


## Types

@docs Import

-}

import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


{-| Type alias representing an Import
-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe (Node Exposing)
    }
