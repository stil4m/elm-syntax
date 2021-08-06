module Elm.Syntax.Import exposing
    ( Import
    , encode, decoder
    )

{-| This syntax represents imports in Elm.
For example:

    import Html.Attributes as HA exposing (style)


## Types

@docs Import


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias representing an Import
-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe (Node Exposing)
    }


{-| Encode a `Import` syntax element to JSON.
-}
encode : Import -> Value
encode { moduleName, moduleAlias, exposingList } =
    JE.object
        [ ( "moduleName", Node.encode ModuleName.encode moduleName )
        , ( "moduleAlias"
          , moduleAlias
                |> Maybe.map (Node.encode ModuleName.encode)
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
decoder : Decoder Import
decoder =
    JD.map3 Import
        (JD.field "moduleName" <| Node.decoder ModuleName.decoder)
        (JD.field "moduleAlias" (JD.nullable <| Node.decoder ModuleName.decoder))
        (JD.field "exposingList" (JD.nullable <| Node.decoder Exposing.decoder))
