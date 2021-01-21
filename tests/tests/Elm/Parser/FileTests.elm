module Elm.Parser.FileTests exposing (all)

import Elm.Internal.RawFile exposing (RawFile(..))
import Elm.Parser as Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as Parser exposing (file)
import Elm.Parser.Samples as Samples
import Elm.Parser.State exposing (emptyState)
import Elm.RawFile as RawFile
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Json.Decode
import Json.Encode
import Test exposing (..)


all : Test
all =
    Test.concat
        [ describe "FileTests" <|
            List.indexedMap
                (\n s ->
                    test ("sample " ++ String.fromInt (n + 1)) <|
                        \() ->
                            parseFullStringState emptyState s Parser.file |> Expect.notEqual Nothing
                )
                Samples.allSamples

        -- , describe "Error messages" <|
        --     [ test "failure on module name" <|
        --         \() ->
        --             Parser.parse "module foo exposing (..)\nx = 1"
        --                 |> Result.toMaybe
        --                 |> Expect.equal Nothing
        --     , test "failure on declaration" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..)\n\ntype x = \n  1"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,0)" ])
        --     , test "failure on declaration expression" <|
        --         \() ->
        --             Parser.parse "module Foo exposing (..) \nx = \n  x + _"
        --                 |> Expect.equal (Err [ "Could not continue parsing on location (2,6)" ])
        --     ]
        , describe "FileTests - serialisation"
            [ Samples.allSamples
                |> List.indexedMap
                    (\n s ->
                        test ("sample " ++ String.fromInt (n + 1)) <|
                            \() ->
                                let
                                    parsed : Maybe RawFile
                                    parsed =
                                        parseFullStringState emptyState s Parser.file
                                            |> Maybe.map Raw

                                    roundTrip : Maybe RawFile.RawFile
                                    roundTrip =
                                        parsed
                                            |> Maybe.map (RawFile.encode >> Json.Encode.encode 0)
                                            |> Maybe.andThen (Json.Decode.decodeString RawFile.decoder >> Result.toMaybe)
                                in
                                Expect.equal parsed roundTrip
                    )
                |> Test.concat
            ]
        ]
