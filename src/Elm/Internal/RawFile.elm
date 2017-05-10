module Elm.Internal.RawFile exposing (RawFile(Raw), fromFile)

import Elm.Syntax.File exposing (File)


type RawFile
    = Raw File


fromFile : File -> RawFile
fromFile =
    Raw
