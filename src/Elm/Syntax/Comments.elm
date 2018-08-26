module Elm.Syntax.Comments exposing (Comment, decode, encode)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias Comment =
    String


encode : Comment -> Value
encode =
    JE.string


decode : Decoder Comment
decode =
    JD.string
