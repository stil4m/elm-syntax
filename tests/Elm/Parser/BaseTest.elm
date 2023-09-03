module Elm.Parser.BaseTest exposing (all)

import Elm.Parser.Base as Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "BaseTest"
        [ test "moduleName" <|
            \() ->
                parseWithFailure "Foo" Parser.moduleName
                    |> Expect.equal (Ok [ "Foo" ])
        , test "moduleNameDir" <|
            \() ->
                parseWithFailure "Foo.Bar" Parser.moduleName
                    |> Expect.equal (Ok [ "Foo", "Bar" ])
        ]
