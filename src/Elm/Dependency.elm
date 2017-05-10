module Elm.Dependency exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Interface exposing (Interface)


type alias Dependency =
    { name : String
    , version : Version
    , interfaces : Dict ModuleName Interface
    }


type alias Version =
    String
