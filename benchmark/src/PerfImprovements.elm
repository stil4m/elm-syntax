module PerfImprovements exposing (main)

{-| This benchmark shows that the Elm compiler is smart enough to understand
that `value.a` and `value |> .a` are the same, and they both compile to the same source code.

In other words, `value |> .a` doesn't create a function call, which is good for performance.

-}

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Elm.Parser
import V7_3_1.Elm.Parser


suite : Benchmark
suite =
    let
        source : String
        source =
            """-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html
module Buttons exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)

main =
  beginnerProgram { model = 0, view = view, update = update }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
"""
    in
    Benchmark.compare "Parsing files"
        "Previous version"
        (\() -> V7_3_1.Elm.Parser.parseToFile source)
        "New version"
        (\() -> Elm.Parser.parseToFile source)


main : BenchmarkProgram
main =
    program suite
