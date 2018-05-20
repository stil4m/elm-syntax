module Test exposing (..)

import Html exposing (Html)
import Html.Attributes


type TestResult
    = Good
    | Bad
    | Warn


type alias Test =
    ( TestResult, Html () )


type alias Expectation =
    Test


resultAsColor r =
    case r of
        Good ->
            "green"

        Bad ->
            "red"

        Warn ->
            "orange"


test : String -> (() -> Expectation) -> Test
test title f =
    let
        ( result, h ) =
            f ()

        () =
            ()
    in
    ( result
    , Html.div
        [ Html.Attributes.style "background" (resultAsColor result)
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "width" "10000px"
        ]
        [ Html.h4 [] [ Html.text title ]
        , h
        ]
    )


concat : List Test -> Test
concat =
    describe ""


describe : String -> List Test -> Test
describe title tests =
    let
        thisTestResult =
            case tests of
                [] ->
                    Good

                _ ->
                    if List.all (Tuple.first >> (==) Good) tests then
                        Good

                    else if List.all (Tuple.first >> (==) Bad) tests then
                        Bad

                    else
                        Warn

        color =
            resultAsColor thisTestResult
    in
    ( thisTestResult
    , Html.div [ Html.Attributes.style "background" color ]
        [ Html.h3 [] [ Html.text title ]
        , Html.div [] (List.map Tuple.second tests)
        ]
    )
