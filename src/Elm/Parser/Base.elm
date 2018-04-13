module Elm.Parser.Base exposing (moduleName, variablePointer)

import Combine exposing ((<$>), Parser, sepBy1, string)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Base exposing (ModuleName, VariablePointer)


variablePointer : Parser State String -> Parser State VariablePointer
variablePointer p =
    withRange (VariablePointer <$> p)


moduleName : Parser s ModuleName
moduleName =
    sepBy1 (string ".") Tokens.typeName
