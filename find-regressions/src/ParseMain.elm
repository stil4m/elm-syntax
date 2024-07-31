port module ParseMain exposing (main)

import Elm.Parser as Parser
import Elm.Syntax.File exposing (File)
import Json.Encode as Encode


port requestParsing : (String -> msg) -> Sub msg


port parseResult : Encode.Value -> Cmd msg


main : Program () () Msg
main =
    Platform.worker
        { init = always ( (), Cmd.none )
        , update = \msg _ -> ( (), update msg )
        , subscriptions = always subscriptions
        }


subscriptions : Sub Msg
subscriptions =
    requestParsing GotFile


type Msg
    = GotFile String


update : Msg -> Cmd Msg
update (GotFile source) =
    let
        json : Encode.Value
        json =
            case Parser.parseToFile source of
                Ok ast ->
                    Elm.Syntax.File.encode ast

                Err _ ->
                    Encode.null
    in
    parseResult json
