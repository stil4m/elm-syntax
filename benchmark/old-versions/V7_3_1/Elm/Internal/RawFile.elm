module V7_3_1.Elm.Internal.RawFile exposing (RawFile(..), fromFile)

import V7_3_1.Elm.Syntax.File exposing (File)


type RawFile
    = Raw File


fromFile : File -> RawFile
fromFile =
    Raw
