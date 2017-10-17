module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing ((*>), (<*>), Parser, maybe, succeed)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken, moduleName)
import Elm.Parser.Util exposing (moreThanIndentWhitespace)
import Elm.Syntax.Module exposing (Import)


importDefinition : Parser State Import
importDefinition =
    withRange <|
        succeed Import
            <*> (importToken *> moreThanIndentWhitespace *> moduleName)
            <*> maybe (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> moduleName)
            <*> exposeDefinition exposable
