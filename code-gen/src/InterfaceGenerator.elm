port module InterfaceGenerator exposing (main)

import Dict exposing (Dict)
import Elm.Dependency exposing (Dependency)
import Elm.Interface exposing (Interface)
import Elm.Parser exposing (parse)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Json.Decode as JD exposing (Value)


port onInterface : String -> Cmd msg


main =
    Platform.worker
        { init = init, update = update, subscriptions = always Sub.none }


type alias Flags =
    { name : String, version : String, files : Value }


type alias Model =
    ()


type alias Msg =
    ()


handleFile : ( String, String ) -> Maybe ( ModuleName, Interface )
handleFile ( rawModuleName, contents ) =
    parse contents
        |> Result.toMaybe
        |> Maybe.map Elm.Interface.build
        |> Maybe.map (Tuple.pair (String.split "." rawModuleName))


init : Flags -> ( (), Cmd Msg )
init { name, version, files } =
    let
        interfaces =
            JD.decodeValue (JD.dict JD.string) files
                |> Result.withDefault Dict.empty
                |> Dict.toList
                |> List.filterMap handleFile
                |> Dict.fromList

        dep : Dependency
        dep =
            { interfaces = interfaces, name = name, version = version }
    in
    ( (), onInterface (Debug.toString dep) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
