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
                TestUtil.parse "Foo" Parser.moduleName
                    |> Maybe.map Node.value
                    |> Expect.equal (Just [ "Foo" ])
        , test "moduleNameDir" <|
            \() ->
                TestUtil.parse "Foo.Bar" Parser.moduleName
                    |> Maybe.map Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
        ]
