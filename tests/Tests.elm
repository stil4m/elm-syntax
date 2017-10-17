module Tests exposing (..)

import Elm.Parser.Tests
import Elm.ProcessingTests
import Test exposing (..)


all : Test
all =
    concat
        [ Elm.Parser.Tests.suite
        , Elm.ProcessingTests.suite
        ]
