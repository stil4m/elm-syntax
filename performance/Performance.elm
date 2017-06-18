port module Performance exposing (main)

import Platform
import Elm.Parser as P
import Elm.Interface as Interface exposing (Interface)


port onContent : (( String, String ) -> msg) -> Sub msg


port pushResult : ( Bool, String ) -> Cmd msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = \_ -> onContent NewContent
        }


type alias Model =
    ()


type Msg
    = NewContent ( String, String )


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update (NewContent ( fn, content )) model =
    case P.parse content of
        Ok rf ->
            let
                x =
                    Ok (Interface.build rf)
            in
                ( ()
                , pushResult ( x == x, fn )
                )

        Err e ->
            ( ()
            , pushResult ( False, fn )
            )
