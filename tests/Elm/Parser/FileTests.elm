module Elm.Parser.FileTests exposing (all)

import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as Parser
import Elm.Parser.Samples as Samples
import Elm.RawFile as RawFile exposing (RawFile)
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
                            parse s Parser.file |> Expect.notEqual Nothing
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
            (Samples.allSamples
                |> List.indexedMap
                    (\n s ->
                        test ("sample " ++ String.fromInt (n + 1)) <|
                            \() ->
                                let
                                    parsed : Maybe RawFile
                                    parsed =
                                        parse s Parser.file
                                            |> Maybe.map InternalRawFile.Raw

                                    roundTrip : Maybe RawFile
                                    roundTrip =
                                        parsed
                                            |> Maybe.map (RawFile.encode >> Json.Encode.encode 0)
                                            |> Maybe.andThen (Json.Decode.decodeString RawFile.decoder >> Result.toMaybe)
                                in
                                Expect.equal parsed roundTrip
                    )
            )
        , test "Comments ordering" <|
            \() ->
                let
                    input : String
                    input =
                        """
module Foo exposing (..)

{-| Module documentation
-}

import A

-- 1
{- 2 -}
-- 3

{-| Function declaration
-}
f =
    -- 4
    identity

-- 5
{- 6 -}
"""
                in
                Elm.Parser.parseToFile input
                    |> Result.map .comments
                    |> Expect.equal
                        (Ok
                            [ Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
                            ]
                        )
        ]
