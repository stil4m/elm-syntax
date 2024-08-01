port module ParseMain exposing (main)

import Elm.Parser as Parser
import Elm.Syntax.File exposing (File)
import Json.Encode as Encode
import Parser exposing (DeadEnd)


port requestParsing : (String -> msg) -> Sub msg


port parseResult : Encode.Value -> Cmd msg


type alias Version =
    String


main : Program Version Version Msg
main =
    Platform.worker
        { init = \version -> ( version, Cmd.none )
        , update = \msg version -> ( version, update version msg )
        , subscriptions = always subscriptions
        }


subscriptions : Sub Msg
subscriptions =
    requestParsing GotFile


type Msg
    = GotFile String


update : Version -> Msg -> Cmd Msg
update version (GotFile source) =
    let
        json : Encode.Value
        json =
            case parseAndBenchmark version source of
                Ok ast ->
                    Elm.Syntax.File.encode ast

                Err _ ->
                    Encode.null
    in
    parseResult json


parseAndBenchmark : Version -> String -> Result (List DeadEnd) File
parseAndBenchmark version source =
    let
        start : ()
        start =
            timeStart version
    in
    Parser.parseToFile source
        |> (\parsed -> always parsed (timeEnd version))


timeStart : Version -> ()
timeStart version =
    ()


timeEnd : Version -> ()
timeEnd version =
    ()
