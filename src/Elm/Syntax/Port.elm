module Elm.Syntax.Port exposing (Port)

{-| This syntax represents ports.
For example:

    {-| This is a port.
    -}
    port send : String -> Cmd msg

    {-| This is another port.
    -}
    port scroll : (Move -> msg) -> Sub msg


## Types

@docs Port

-}

import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)


{-| Information associated with a Port.
A bit meta, but you get the idea. All information that you can define in a type alias is embedded.
-}
type alias Port =
    { documentation : Maybe (Node Documentation)
    , signature : Node Signature
    }
