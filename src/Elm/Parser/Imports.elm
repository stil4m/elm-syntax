module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing ((*>), (<*>), Parser, maybe, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Module exposing (Import)


importDefinition : Parser State Import
importDefinition =
    withRange <|
        succeed Import
            <*> (importToken *> Layout.layout *> moduleName)
            <*> maybe (Layout.layout *> asToken *> Layout.layout *> moduleName)
            <*> maybe (exposeDefinition exposable)
