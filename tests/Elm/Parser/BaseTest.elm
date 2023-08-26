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
                parse "Foo" Parser.moduleName
                    |> Expect.equal (Just <| [ "Foo" ])
        , test "moduleNameDir" <|
            \() ->
                parse "Foo.Bar" Parser.moduleName
                    |> Expect.equal (Just <| [ "Foo", "Bar" ])
        ]
