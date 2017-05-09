module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser, succeed, (<*>), (*>), maybe)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Tokens exposing (importToken, moduleName, asToken)
import Elm.Syntax.Module exposing (Import)
import Elm.Parser.Util exposing (moreThanIndentWhitespace)
import Elm.Parser.State exposing (State)
import Elm.Parser.Ranges exposing (withRange)


importDefinition : Parser State Import
importDefinition =
    withRange <|
        succeed Import
            <*> (importToken *> moreThanIndentWhitespace *> moduleName)
            <*> maybe (moreThanIndentWhitespace *> asToken *> moreThanIndentWhitespace *> moduleName)
            <*> exposeDefinition exposable
