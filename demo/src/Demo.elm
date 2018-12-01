module Demo exposing (main)

import Browser
import Elm.Parser
import Elm.Processing
import Elm.RawFile as Elm
import Elm.Syntax.File exposing (File)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Json.Encode exposing (Value)
import JsonTree
import NodeCollector


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type Msg
    = Change String
    | NoOp
    | TreeMsg JsonTree.State
    | SelectPath JsonTree.KeyPath


type alias Model =
    { src : String
    , parseResult :
        Maybe (Result (List String) File)
    , value : Value
    , treeState : JsonTree.State
    , treeValue : Maybe JsonTree.Node
    , path : Maybe JsonTree.KeyPath
    }


init : Model
init =
    { src = ""
    , parseResult = Nothing
    , treeValue = Nothing
    , value = Json.Encode.null
    , treeState = JsonTree.defaultState
    , path = Nothing
    }
        |> update (Change """module Foo exposing (foo)

-- some comment

{-| Docs -}
foo = 1
""")


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPath p ->
            { model | path = Just p }

        TreeMsg s ->
            { model | treeState = s }

        NoOp ->
            model

        Change v ->
            let
                parseResult =
                    Elm.Parser.parse v
                        |> Result.map (Elm.Processing.process Elm.Processing.init)

                value =
                    parseResult
                        |> Result.toMaybe
                        |> Maybe.map Elm.Syntax.File.encode
                        |> Maybe.withDefault Json.Encode.null
            in
            { model
                | src = v
                , parseResult = Just parseResult
                , value = value
                , treeValue = JsonTree.parseValue value |> Result.toMaybe
                , treeState = JsonTree.defaultState
            }


config =
    { onSelect = Just SelectPath
    , toMsg = TreeMsg
    }


view : Model -> Html Msg
view m =
    Html.div []
        [ Html.div []
            [ Html.textarea
                [ Html.Events.onInput Change
                , Html.Attributes.value m.src
                , Html.Attributes.cols 50
                , Html.Attributes.rows 20
                , Html.Attributes.style "font-family" "Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace"
                ]
                []
            ]
        , m.parseResult
            |> Maybe.andThen Result.toMaybe
            |> Maybe.map NodeCollector.collect
            |> Maybe.withDefault []
            |> List.map (\v -> Html.li [] [ Html.text (Debug.toString v) ])
            |> Html.ul []
        , m.treeValue
            |> Maybe.map (\tree -> JsonTree.view tree config m.treeState)
            |> Maybe.withDefault (text "Failed to parse JSON")
        , Html.div [] [ Html.text (Debug.toString m) ]
        ]
