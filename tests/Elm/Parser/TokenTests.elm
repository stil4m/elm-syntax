module Elm.Parser.TokenTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Tokens as Parser
import Expect
import Test exposing (..)


longString : String
longString =
    "\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\""


longMultiLineString : String
longMultiLineString =
    "\"\"\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\"\"\""


all : Test
all =
    describe "TokenTests"
        [ test "functionName" <|
            \() ->
                parse "foo" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "foo")
        , test "functionName may not be a keyword" <|
            \() ->
                parse "type" (Combine.fromCore Parser.functionName)
                    |> Expect.equal Nothing
        , test "functionName may be a keyword suffixed with an underscore" <|
            \() ->
                parse "type_" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "type_")
        , test "functionName not empty" <|
            \() ->
                parse "" (Combine.fromCore Parser.functionName)
                    |> Expect.equal Nothing
        , test "functionName with number" <|
            \() ->
                parse "n1" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "n1")
        , test "alias can be a functionName (it is not reserved)" <|
            \() ->
                parse "alias" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "alias")
        , test "infix can be a functionName (it is not reserved)" <|
            \() ->
                parse "infix" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "infix")
        , test "functionName is not matched with 'if'" <|
            \() ->
                parse "if" (Combine.fromCore Parser.functionName)
                    |> Expect.equal Nothing
        , test "functionName with _" <|
            \() ->
                parse "foo_" (Combine.fromCore Parser.functionName)
                    |> Expect.equal (Just "foo_")
        , test "typeName" <|
            \() ->
                parse "MyCmd" (Combine.fromCore Parser.typeName)
                    |> Expect.equal (Just "MyCmd")
        , test "typeName not empty" <|
            \() ->
                parse "" (Combine.fromCore Parser.typeName)
                    |> Expect.equal Nothing
        , test "typeName with number" <|
            \() ->
                parse "T1" (Combine.fromCore Parser.typeName)
                    |> Expect.equal (Just "T1")
        , test "moduleToken" <|
            \() ->
                parse "module" (Combine.fromCore Parser.moduleToken)
                    |> Expect.equal (Just ())
        , test "exposingToken" <|
            \() ->
                parse "exposing" (Combine.fromCore Parser.exposingToken)
                    |> Expect.equal (Just ())
        , test "operatorToken 11 -- is not an operator" <|
            \() ->
                parse "--" (Combine.fromCore Parser.prefixOperatorToken)
                    |> Expect.equal Nothing
        , test "operatorToken 14" <|
            \() ->
                parse "=" (Combine.fromCore Parser.prefixOperatorToken)
                    |> Expect.equal Nothing
        , test "operatorToken 15" <|
            \() ->
                parse "?" (Combine.fromCore Parser.prefixOperatorToken)
                    |> Expect.equal Nothing
        , test "multiline string" <|
            \() ->
                parse "\"\"\"Bar foo \n a\"\"\"" (Combine.fromCore Parser.multiLineStringLiteral)
                    |> Expect.equal (Just "Bar foo \n a")
        , test "multiline string escape" <|
            \() ->
                parse """\"\"\" \\\"\"\" \"\"\"""" (Combine.fromCore Parser.multiLineStringLiteral)
                    |> Expect.equal (Just """ \"\"\" """)
        , test "character escaped" <|
            \() ->
                parse "'\\''" (Combine.fromCore Parser.characterLiteral)
                    |> Expect.equal (Just '\'')
        , test "character escaped - 2" <|
            \() ->
                parse "'\\r'" (Combine.fromCore Parser.characterLiteral)
                    |> Expect.equal (Just '\u{000D}')
        , test "unicode char" <|
            \() ->
                parse "'\\u{000D}'" (Combine.fromCore Parser.characterLiteral)
                    |> Expect.equal (Just '\u{000D}')
        , test "unicode char with lowercase hex" <|
            \() ->
                parse "'\\u{000d}'" (Combine.fromCore Parser.characterLiteral)
                    |> Expect.equal (Just '\u{000D}')
        , test "string escaped 3" <|
            \() ->
                parse "\"\\\"\"" (Combine.fromCore Parser.stringLiteral)
                    |> Expect.equal (Just "\"")
        , test "string escaped" <|
            \() ->
                parse "\"foo\\\\\"" (Combine.fromCore Parser.stringLiteral)
                    |> Expect.equal (Just "foo\\")
        , test "character escaped 3" <|
            \() ->
                parse "'\\n'" (Combine.fromCore Parser.characterLiteral)
                    |> Expect.equal (Just '\n')
        , test "long string" <|
            \() ->
                parse longString (Combine.fromCore Parser.stringLiteral)
                    |> Expect.notEqual Nothing
        , test "long multi line string" <|
            \() ->
                parse longMultiLineString (Combine.fromCore Parser.multiLineStringLiteral)
                    |> Expect.notEqual Nothing
        , test "ρ function" <|
            \() ->
                parse "ρ" (Combine.fromCore Parser.functionName)
                    |> Expect.notEqual Nothing
        , test "ε2 function" <|
            \() ->
                parse "ε2" (Combine.fromCore Parser.functionName)
                    |> Expect.notEqual Nothing
        , test "εε function" <|
            \() ->
                parse "εε" (Combine.fromCore Parser.functionName)
                    |> Expect.notEqual Nothing
        , test "ρ uppercase function" <|
            \() ->
                parse (String.toUpper "ρ") (Combine.fromCore Parser.functionName)
                    |> Expect.equal Nothing
        , test "ε uppercase function" <|
            \() ->
                parse (String.toUpper "ε") (Combine.fromCore Parser.functionName)
                    |> Expect.equal Nothing
        , test "ρ type name" <|
            \() ->
                parse "ρ" (Combine.fromCore Parser.typeName)
                    |> Expect.equal Nothing
        , test "ε2 type name" <|
            \() ->
                parse "ε2" (Combine.fromCore Parser.typeName)
                    |> Expect.equal Nothing
        , test "εε type name" <|
            \() ->
                parse "εε" (Combine.fromCore Parser.typeName)
                    |> Expect.equal Nothing
        , test "ρ uppercase type name" <|
            \() ->
                parse (String.toUpper "ρ") (Combine.fromCore Parser.typeName)
                    |> Expect.notEqual Nothing
        , test "ε uppercase type name" <|
            \() ->
                parse (String.toUpper "ε") (Combine.fromCore Parser.typeName)
                    |> Expect.notEqual Nothing
        ]
