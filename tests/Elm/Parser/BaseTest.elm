module Elm.Parser.BaseTest exposing (all)

import Elm.Parser.Base as Parser
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Syntax.Node as Node
import Expect
import Test exposing (..)


all : Test
all =
    describe "BaseTest"
        [ test "moduleName" <|
            \() ->
                parseWithFailure "Foo" Parser.moduleName
                    |> Result.map Node.value
                    |> Expect.equal (Ok [ "Foo" ])
        , test "moduleNameDir" <|
            \() ->
                parseWithFailure "Foo.Bar" Parser.moduleName
                    |> Result.map Node.value
                    |> Expect.equal (Ok [ "Foo", "Bar" ])
        ]
