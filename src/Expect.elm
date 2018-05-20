module Expect exposing (..)

import Html exposing (Html)
import Test exposing (Expectation, TestResult(..))


equal : a -> a -> Expectation
equal x y =
    if x == y then
        ( Good
        , Html.div []
            [ Html.text "Good" ]
        )

    else
        ( Bad
        , Html.div []
            [ Html.text "Bad"
            , Html.hr [] []
            , Html.h4 [] [ Html.text "Expected:" ]
            , Html.pre [] [ Html.text (Debug.toString x) ]
            , Html.hr [] []
            , Html.h4 [] [ Html.text "But got:" ]
            , Html.pre [] [ Html.text (Debug.toString y) ]
            ]
        )


notEqual : a -> a -> Expectation
notEqual x y =
    if x /= y then
        ( Good
        , Html.div []
            [ Html.text "Good" ]
        )

    else
        ( Bad
        , Html.div []
            [ Html.text "Bad"
            , Html.hr [] []
            , Html.h4 [] [ Html.text "Not expected:" ]
            , Html.pre [] [ Html.text (Debug.toString x) ]
            , Html.hr [] []
            , Html.h4 [] [ Html.text "But got:" ]
            , Html.pre [] [ Html.text (Debug.toString y) ]
            ]
        )
