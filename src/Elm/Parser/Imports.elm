module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser, maybe, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Module exposing (Import)


importDefinition : Parser State Import
importDefinition =
    let
        importAndModuleName =
            importToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleName

        asDefinition =
            Layout.layout
                |> Combine.continueWith asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleName

        importExposing =
            exposeDefinition exposable
    in
    withRange <|
        (succeed Import
            |> Combine.andMap importAndModuleName
            |> Combine.andMap (maybe asDefinition)
            |> Combine.andMap (maybe importExposing)
        )
