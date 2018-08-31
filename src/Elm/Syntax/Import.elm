module Elm.Syntax.Import exposing
    ( Import
    , encode, decoder
    )

{-| Import Syntax


# Types

@docs Import


# Serialization

@docs encode, decoder

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Import definition
-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe Exposing
    }


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
                |> Maybe.map Exposing.encode
                |> Maybe.withDefault JE.null
          )
        ]


decoder : Decoder Import
decoder =
    JD.map3 Import
        (JD.field "moduleName" <| Node.decoder ModuleName.decoder)
        (JD.field "moduleAlias" (JD.nullable <| Node.decoder ModuleName.decoder))
        (JD.field "exposingList" (JD.nullable Exposing.decoder))
