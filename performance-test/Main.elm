port module Main exposing (main)

import Elm.Parser
import Platform


port onFile : (String -> msg) -> Sub msg


port failedParse : String -> Cmd msg


port successParse : String -> Cmd msg


type Model
    = Model


type Msg
    = OnContent String


init : ( Model, Cmd Msg )
init =
    ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnContent s ->
            case Elm.Parser.parse s of
                Ok v ->
                    ( model, successParse "DONE" )

                Err e ->
                    ( model, failedParse s )


subscriptions : Model -> Sub Msg
subscriptions =
    always (onFile OnContent)


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
