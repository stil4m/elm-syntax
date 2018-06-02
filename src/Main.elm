module Main exposing (main)

import Elm.Parser
import Platform


type Model
    = Model


type Msg
    = OnContent String


type alias Flags =
    { body : String
    , name : String
    }


init : Flags -> ( Model, Cmd Msg )
init { body, name } =
    case Elm.Parser.parse body of
        Ok v ->
            let
                _ =
                    Debug.log "Success" <| always name <| v
            in
            ( Model
            , Cmd.none
            )

        Err e ->
            let
                _ =
                    Debug.log "Error" <| always name <| e
            in
            ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
