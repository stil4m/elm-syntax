module Elm.Dependency exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Base exposing (ModuleName)
import Elm.Syntax.Infix exposing (Infix)


type alias Dependency =
    { name : String
    , version : Version
    , interfaces : Dict ModuleName Interface
    }


type alias Version =
    String


type alias Interface =
    List Exposed


type Exposed
    = Function String
    | Type ( String, List String )
    | Alias String
    | Operator Infix
