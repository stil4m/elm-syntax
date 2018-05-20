module Main exposing (main)

import Elm.Parser
import Platform


type Model
    = Model


type Msg
    = OnContent String


init : String -> ( Model, Cmd Msg )
init s =
    case Elm.Parser.parse s of
        Ok v ->
            let
                _ =
                    Debug.log "Success!" <| always () <| v
            in
            ( Model
            , Cmd.none
            )

        Err e ->
            let
                _ =
                    Debug.log "Error!" <| always () <| e
            in
            ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
