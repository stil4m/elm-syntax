module Elm.Parser.TokenTests exposing (all)

import Elm.Parser.TestUtil exposing (..)
import Elm.Parser.Tokens as Parser
import Elm.Syntax.StringLiteralType exposing (StringLiteralType(..))
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
                parse "foo" Parser.functionName
                    |> Expect.equal (Just "foo")
        , test "functionName may not be a keyword" <|
            \() ->
                parse "type" Parser.functionName
                    |> Expect.equal Nothing
        , test "functionName may be a keyword suffixed with an underscore" <|
            \() ->
                parse "type_" Parser.functionName
                    |> Expect.equal (Just "type_")
        , test "functionName not empty" <|
            \() ->
                parse "" Parser.functionName
                    |> Expect.equal Nothing
        , test "functionName with number" <|
            \() ->
                parse "n1" Parser.functionName
                    |> Expect.equal (Just "n1")
        , test "alias can be a functionName (it is not reserved)" <|
            \() ->
                parse "alias" Parser.functionName
                    |> Expect.equal (Just "alias")
        , test "infix can be a functionName (it is not reserved)" <|
            \() ->
                parse "infix" Parser.functionName
                    |> Expect.equal (Just "infix")
        , test "functionName is not matched with 'if'" <|
            \() ->
                parse "if" Parser.functionName
                    |> Expect.equal Nothing
        , test "functionName with _" <|
            \() ->
                parse "foo_" Parser.functionName
                    |> Expect.equal (Just "foo_")
        , test "typeName" <|
            \() ->
                parse "MyCmd" Parser.typeName
                    |> Expect.equal (Just "MyCmd")
        , test "typeName not empty" <|
            \() ->
                parse "" Parser.typeName
                    |> Expect.equal Nothing
        , test "typeName with number" <|
            \() ->
                parse "T1" Parser.typeName
                    |> Expect.equal (Just "T1")
        , test "moduleToken" <|
            \() ->
                parse "module" Parser.moduleToken
                    |> Expect.equal (Just ())
        , test "exposingToken" <|
            \() ->
                parse "exposing" Parser.exposingToken
                    |> Expect.equal (Just ())
        , test "operatorToken 11 -- is not an operator" <|
            \() ->
                parse "--" Parser.prefixOperatorToken
                    |> Expect.equal Nothing
        , test "operatorToken 14" <|
            \() ->
                parse "=" Parser.prefixOperatorToken
                    |> Expect.equal Nothing
        , test "operatorToken 15" <|
            \() ->
                parse "?" Parser.prefixOperatorToken
                    |> Expect.equal Nothing
        , test "multiline string" <|
            \() ->
                parse "\"\"\"Bar foo \n a\"\"\"" Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.equal (Just ( TripleQuote, "Bar foo \n a" ))
        , test "multiline string escape" <|
            \() ->
                parse """\"\"\" \\\"\"\" \"\"\"""" Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.equal (Just ( TripleQuote, """ \"\"\" """ ))
        , test "character escaped" <|
            \() ->
                parse "'\\''" Parser.characterLiteral
                    |> Expect.equal (Just '\'')
        , test "character escaped - 2" <|
            \() ->
                parse "'\\r'" Parser.characterLiteral
                    |> Expect.equal (Just '\u{000D}')
        , test "unicode char" <|
            \() ->
                parse "'\\u{000D}'" Parser.characterLiteral
                    |> Expect.equal (Just '\u{000D}')
        , test "unicode char with lowercase hex" <|
            \() ->
                parse "'\\u{000d}'" Parser.characterLiteral
                    |> Expect.equal (Just '\u{000D}')
        , test "string escaped 3" <|
            \() ->
                parse "\"\\\"\"" Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.equal (Just ( SingleQuote, "\"" ))
        , test "string escaped" <|
            \() ->
                parse "\"foo\\\\\"" Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.equal (Just ( SingleQuote, "foo\\" ))
        , test "character escaped 3" <|
            \() ->
                parse "'\\n'" Parser.characterLiteral
                    |> Expect.equal (Just '\n')
        , test "long string" <|
            \() ->
                parse longString Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.notEqual Nothing
        , test "long multi line string" <|
            \() ->
                parse longMultiLineString Parser.singleOrTripleQuotedStringLiteral
                    |> Expect.notEqual Nothing
        , test "ρ function" <|
            \() ->
                parse "ρ" Parser.functionName
                    |> Expect.notEqual Nothing
        , test "ε2 function" <|
            \() ->
                parse "ε2" Parser.functionName
                    |> Expect.notEqual Nothing
        , test "εε function" <|
            \() ->
                parse "εε" Parser.functionName
                    |> Expect.notEqual Nothing
        , test "ρ uppercase function" <|
            \() ->
                parse (String.toUpper "ρ") Parser.functionName
                    |> Expect.equal Nothing
        , test "ε uppercase function" <|
            \() ->
                parse (String.toUpper "ε") Parser.functionName
                    |> Expect.equal Nothing
        , test "ρ type name" <|
            \() ->
                parse "ρ" Parser.typeName
                    |> Expect.equal Nothing
        , test "ε2 type name" <|
            \() ->
                parse "ε2" Parser.typeName
                    |> Expect.equal Nothing
        , test "εε type name" <|
            \() ->
                parse "εε" Parser.typeName
                    |> Expect.equal Nothing
        , test "ρ uppercase type name" <|
            \() ->
                parse (String.toUpper "ρ") Parser.typeName
                    |> Expect.notEqual Nothing
        , test "ε uppercase type name" <|
            \() ->
                parse (String.toUpper "ε") Parser.typeName
                    |> Expect.notEqual Nothing
        ]
