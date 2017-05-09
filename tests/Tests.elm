module Tests exposing (..)

import Test exposing (..)
import Elm.Parser.Tests


all : Test
all =
    concat [ Elm.Parser.Tests.suite ]
