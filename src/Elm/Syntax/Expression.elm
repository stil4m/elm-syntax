module Elm.Syntax.Expression exposing
    ( Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation
    , functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication
    , encode, encodeFunction, decoder, functionDecoder
    )

{-|


# Expression Syntax

This syntax represens all that you can express in Elm.
Although it is a easy and simple language, you can express a lot! See the `Expression` type for all the things you can express.


## Types

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation


## Functions

@docs functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication


## Serialization

@docs encode, encodeFunction, decoder, functionDecoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Infix as Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Node Documentation)
    , signature : Maybe (Node Signature)
    , declaration : Node FunctionImplementation
    }


{-| Get the full range of a function
-}
functionRange : Function -> Range
functionRange function =
    Range.combine
        [ case function.documentation of
            Just documentation ->
                Node.range documentation

            Nothing ->
                function.signature
                    |> Maybe.map
                        (\(Node _ value) ->
                            case value.name of
                                Node r _ ->
                                    r
                        )
                    |> Maybe.withDefault (function.declaration |> Node.value |> .name |> (\(Node r _) -> r))
        , (\(Node r _) -> r) (Node.value function.declaration).expression
        ]


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node Pattern)
    , expression : Node Expression
    }


{-| Wrapper type for an expresion on a certain range
-}
type Expression
    = UnitExpr
    | Application (List (Node Expression))
    | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
    | FunctionOrValue ModuleName String
    | IfBlock (Node Expression) (Node Expression) (Node Expression)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (Node Expression)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (Node Expression))
    | ParenthesizedExpression (Node Expression)
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List (Node RecordSetter))
    | ListExpr (List (Node Expression))
    | RecordAccess (Node Expression) (Node String)
    | RecordAccessFunction String
    | RecordUpdateExpression (Node String) (List (Node RecordSetter))
    | GLSLExpression String


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Node String, Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Node LetDeclaration)
    , expression : Node Expression
    }


{-| Union type for all possible declations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Node Pattern) (Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Node Pattern)
    , expression : Node Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Node Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Node Pattern, Node Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case


{-| Check whether an expression is a lambda-expression
-}
isLambda : Expression -> Bool
isLambda e =
    case e of
        LambdaExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is a let-expression
-}
isLet : Expression -> Bool
isLet e =
    case e of
        LetExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an if-else-expression
-}
isIfElse : Expression -> Bool
isIfElse e =
    case e of
        IfBlock _ _ _ ->
            True

        _ ->
            False


{-| Check whether an expression is a case-expression
-}
isCase : Expression -> Bool
isCase e =
    case e of
        CaseExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an operator appliation expression
-}
isOperatorApplication : Expression -> Bool
isOperatorApplication e =
    case e of
        OperatorApplication _ _ _ _ ->
            True

        _ ->
            False



-- Serialization


{-| Encode an `Expression` syntax element to JSON.
-}
encode : Expression -> Value
encode expr =
    case expr of
        UnitExpr ->
            encodeTyped "unit" JE.null

        Application l ->
            encodeTyped "application" (JE.list (Node.encode encode) l)

        OperatorApplication op dir left right ->
            encodeTyped "operatorapplication" (encodeOperatorApplication op dir left right)

        FunctionOrValue moduleName name ->
            encodeTyped "functionOrValue"
                (JE.object
                    [ ( "moduleName", ModuleName.encode moduleName )
                    , ( "name", JE.string name )
                    ]
                )

        IfBlock c t e ->
            encodeTyped "ifBlock" <|
                JE.object
                    [ ( "clause", Node.encode encode c )
                    , ( "then", Node.encode encode t )
                    , ( "else", Node.encode encode e )
                    ]

        PrefixOperator x ->
            encodeTyped "prefixoperator" (JE.string x)

        Operator x ->
            encodeTyped "operator" (JE.string x)

        Hex h ->
            encodeTyped "hex" (JE.int h)

        Integer x ->
            encodeTyped "integer" (JE.int x)

        Floatable x ->
            encodeTyped "float" (JE.float x)

        Negation x ->
            encodeTyped "negation" (Node.encode encode x)

        Literal x ->
            encodeTyped "literal" (JE.string x)

        CharLiteral c ->
            encodeTyped "charLiteral" (JE.string <| String.fromChar c)

        TupledExpression xs ->
            encodeTyped "tupled" (JE.list (Node.encode encode) xs)

        ListExpr xs ->
            encodeTyped "list" (JE.list (Node.encode encode) xs)

        ParenthesizedExpression x ->
            encodeTyped "parenthesized" (Node.encode encode x)

        LetExpression x ->
            encodeTyped "let" <| encodeLetBlock x

        CaseExpression x ->
            encodeTyped "case" <| encodeCaseBlock x

        LambdaExpression x ->
            encodeTyped "lambda" <| encodeLambda x

        RecordAccess exp name ->
            encodeTyped "recordAccess" <|
                JE.object
                    [ ( "expression", Node.encode encode exp )
                    , ( "name", Node.encode JE.string name )
                    ]

        RecordAccessFunction x ->
            encodeTyped "recordAccessFunction" (JE.string x)

        RecordExpr xs ->
            encodeTyped "record" (JE.list (Node.encode encodeRecordSetter) xs)

        RecordUpdateExpression name updates ->
            encodeTyped "recordUpdate" (encodeRecordUpdate name updates)

        GLSLExpression x ->
            encodeTyped "glsl" (JE.string x)


encodeOperatorApplication : String -> InfixDirection -> Node Expression -> Node Expression -> Value
encodeOperatorApplication operator direction left right =
    JE.object
        [ ( "operator", JE.string operator )
        , ( "direction", Infix.encodeDirection direction )
        , ( "left", Node.encode encode left )
        , ( "right", Node.encode encode right )
        ]


encodeLetBlock : LetBlock -> Value
encodeLetBlock { declarations, expression } =
    JE.object
        [ ( "declarations", JE.list (Node.encode encodeLetDeclaration) declarations )
        , ( "expression", Node.encode encode expression )
        ]


encodeRecordUpdate : Node String -> List (Node RecordSetter) -> Value
encodeRecordUpdate name updates =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "updates", JE.list (Node.encode encodeRecordSetter) updates )
        ]


encodeRecordSetter : RecordSetter -> Value
encodeRecordSetter ( field, expression ) =
    JE.object
        [ ( "field", Node.encode JE.string field )
        , ( "expression", Node.encode encode expression )
        ]


encodeLetDeclaration : LetDeclaration -> Value
encodeLetDeclaration letDeclaration =
    case letDeclaration of
        LetFunction f ->
            encodeTyped "function" (encodeFunction f)

        LetDestructuring pattern expression ->
            encodeTyped "destructuring" (encodeDestructuring pattern expression)


{-| Encode a `Function` syntax element to JSON.
-}
encodeFunction : Function -> Value
encodeFunction { documentation, signature, declaration } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "signature", Maybe.map (Node.encode Signature.encode) signature |> Maybe.withDefault JE.null )
        , ( "declaration", Node.encode encodeFunctionDeclaration declaration )
        ]


encodeFunctionDeclaration : FunctionImplementation -> Value
encodeFunctionDeclaration { name, arguments, expression } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "arguments", JE.list (Node.encode Pattern.encode) arguments )
        , ( "expression", Node.encode encode expression )
        ]


encodeDestructuring : Node Pattern -> Node Expression -> Value
encodeDestructuring pattern expression =
    JE.object
        [ ( "pattern", Node.encode Pattern.encode pattern )
        , ( "expression", Node.encode encode expression )
        ]


encodeCaseBlock : CaseBlock -> Value
encodeCaseBlock { cases, expression } =
    JE.object
        [ ( "cases", JE.list encodeCase cases )
        , ( "expression", Node.encode encode expression )
        ]


encodeCase : Case -> Value
encodeCase ( pattern, expression ) =
    JE.object
        [ ( "pattern", Node.encode Pattern.encode pattern )
        , ( "expression", Node.encode encode expression )
        ]


encodeLambda : Lambda -> Value
encodeLambda { args, expression } =
    JE.object
        [ ( "patterns", JE.list (Node.encode Pattern.encode) args )
        , ( "expression", Node.encode encode expression )
        ]


decodeNested : Decoder (Node Expression)
decodeNested =
    JD.lazy (\() -> Node.decoder decoder)


{-| JSON decoder for an `Expression` syntax element.
-}
decoder : Decoder Expression
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "unit", JD.succeed UnitExpr )
                , ( "application", JD.list decodeNested |> JD.map Application )
                , ( "operatorapplication", decodeOperatorApplication )
                , ( "functionOrValue", JD.map2 FunctionOrValue (JD.field "moduleName" ModuleName.decoder) (JD.field "name" JD.string) )
                , ( "ifBlock", JD.map3 IfBlock (JD.field "clause" decodeNested) (JD.field "then" decodeNested) (JD.field "else" decodeNested) )
                , ( "prefixoperator", JD.string |> JD.map PrefixOperator )
                , ( "operator", JD.string |> JD.map Operator )
                , ( "hex", JD.int |> JD.map Hex )
                , ( "integer", JD.int |> JD.map Integer )
                , ( "float", JD.float |> JD.map Floatable )
                , ( "negation", decodeNested |> JD.map Negation )
                , ( "literal", JD.string |> JD.map Literal )
                , ( "charLiteral", decodeChar |> JD.map CharLiteral )
                , ( "tupled", JD.list decodeNested |> JD.map TupledExpression )
                , ( "list", JD.list decodeNested |> JD.map ListExpr )
                , ( "parenthesized", decodeNested |> JD.map ParenthesizedExpression )
                , ( "let", decodeLetBlock |> JD.map LetExpression )
                , ( "case", decodeCaseBlock |> JD.map CaseExpression )
                , ( "lambda", decodeLambda |> JD.map LambdaExpression )
                , ( "recordAccess", JD.map2 RecordAccess (JD.field "expression" decodeNested) (JD.field "name" (Node.decoder JD.string)) )
                , ( "recordAccessFunction", JD.string |> JD.map RecordAccessFunction )
                , ( "record", JD.list (Node.decoder decodeRecordSetter) |> JD.map RecordExpr )
                , ( "recordUpdate"
                  , JD.map2 RecordUpdateExpression
                        (JD.field "name" <| Node.decoder JD.string)
                        (JD.field "updates" (JD.list <| Node.decoder decodeRecordSetter))
                  )
                , ( "glsl", JD.string |> JD.map GLSLExpression )
                ]
        )


decodeRecordSetter : Decoder RecordSetter
decodeRecordSetter =
    JD.lazy
        (\() ->
            JD.map2 Tuple.pair
                (JD.field "field" <| Node.decoder JD.string)
                (JD.field "expression" decodeNested)
        )


decodeLambda : Decoder Lambda
decodeLambda =
    JD.lazy
        (\() ->
            JD.map2 Lambda
                (JD.field "patterns" (JD.list (Node.decoder Pattern.decoder)))
                (JD.field "expression" decodeNested)
        )


decodeCaseBlock : Decoder CaseBlock
decodeCaseBlock =
    JD.lazy
        (\() ->
            JD.map2 CaseBlock
                (JD.field "expression" decodeNested)
                (JD.field "cases" (JD.list decodeCase))
        )


decodeCase : Decoder Case
decodeCase =
    JD.lazy
        (\() ->
            JD.map2 Tuple.pair
                (JD.field "pattern" (Node.decoder Pattern.decoder))
                (JD.field "expression" decodeNested)
        )


decodeLetBlock : Decoder LetBlock
decodeLetBlock =
    JD.lazy
        (\() ->
            JD.map2 LetBlock
                (JD.field "declarations" (JD.list decodeLetDeclaration))
                (JD.field "expression" decodeNested)
        )


decodeLetDeclaration : Decoder (Node LetDeclaration)
decodeLetDeclaration =
    JD.lazy
        (\() ->
            Node.decoder
                (decodeTyped
                    [ ( "function", JD.map LetFunction functionDecoder )
                    , ( "destructuring", JD.map2 LetDestructuring (JD.field "pattern" (Node.decoder Pattern.decoder)) (JD.field "expression" decodeNested) )
                    ]
                )
        )


decodeOperatorApplication : Decoder Expression
decodeOperatorApplication =
    JD.lazy
        (\() ->
            JD.map4 OperatorApplication
                (JD.field "operator" JD.string)
                (JD.field "direction" Infix.decodeDirection)
                (JD.field "left" decodeNested)
                (JD.field "right" decodeNested)
        )


decodeChar : Decoder Char
decodeChar =
    JD.string
        |> JD.andThen
            (\s ->
                case String.uncons s of
                    Just ( c, _ ) ->
                        JD.succeed c

                    Nothing ->
                        JD.fail "Not a char"
            )


{-| JSON decoder for an `Function` syntax element.
-}
functionDecoder : Decoder Function
functionDecoder =
    JD.lazy
        (\() ->
            JD.map3 Function
                (JD.field "documentation" (JD.nullable <| Node.decoder Documentation.decoder))
                (JD.field "signature" (JD.nullable (Node.decoder Signature.decoder)))
                (JD.field "declaration" (Node.decoder decodeFunctionDeclaration))
        )


decodeFunctionDeclaration : Decoder FunctionImplementation
decodeFunctionDeclaration =
    JD.lazy
        (\() ->
            JD.map3 FunctionImplementation
                (JD.field "name" (Node.decoder JD.string))
                (JD.field "arguments" (JD.list (Node.decoder Pattern.decoder)))
                (JD.field "expression" decodeNested)
        )
