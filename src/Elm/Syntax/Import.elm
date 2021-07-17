module Elm.Syntax.Import exposing
    ( Import
    , encode, decoder
    )

{-|


# Import Syntax

This syntax represents imports in Elm.
For example:

    import Html.Attributes as HA exposing (style)


# Types

@docs Import


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias representing an Import
-}
type alias Import r =
    { moduleName : Node r ModuleName
    , moduleAlias : Maybe (Node r String)
    , exposingList : Maybe (Node r (Exposing r))
    }


{-| Encode a `Import` syntax element to JSON.
-}
encode : Import r -> Value
encode { moduleName, moduleAlias, exposingList } =
    JE.object
        [ ( "moduleName", Node.encode ModuleName.encode moduleName )
        , ( "moduleAlias"
          , moduleAlias
                |> Maybe.map (Node.encode JE.string)
                |> Maybe.withDefault JE.null
          )
        , ( "exposingList"
          , exposingList
                |> Maybe.map (Node.encode Exposing.encode)
                |> Maybe.withDefault JE.null
          )
        ]


{-| JSON decoder for a `Import` syntax element.
-}
decoder : Decoder (Import r)
decoder =
    JD.map3 Import
        (JD.field "moduleName" <| Node.decoder ModuleName.decoder)
        (JD.field "moduleAlias" (JD.nullable <| Node.decoder JD.string))
        (JD.field "exposingList" (JD.nullable <| Node.decoder Exposing.decoder))
