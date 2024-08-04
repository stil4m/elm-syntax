module Elm.Parser.BaseTest exposing (all)

import Elm.Parser.Base as Parser
import Elm.Parser.TestUtil as TestUtil
import Elm.Syntax.Node as Node
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "BaseTest"
        [ test "moduleName" <|
            \() ->
                TestUtil.parseToResult "Foo" Parser.moduleName
                    |> Result.map Node.value
                    |> Expect.equal (Ok [ "Foo" ])
        , test "moduleNameDir" <|
            \() ->
                TestUtil.parseToResult "Foo.Bar" Parser.moduleName
                    |> Result.map Node.value
                    |> Expect.equal (Ok [ "Foo", "Bar" ])
        ]
