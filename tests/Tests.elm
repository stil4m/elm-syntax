module Tests exposing (..)

import Test exposing (..)
import Elm.Parser.Tests
import Elm.ProcessingTests


all : Test
all =
    concat
        [ Elm.Parser.Tests.suite
        , Elm.ProcessingTests.suite
        ]
