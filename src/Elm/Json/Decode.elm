module Elm.Json.Decode exposing (decode)

{-|


# Elm.Json.Decode

Decoding Elm Code from Json

@docs decode

-}

import Elm.Internal.RawFile exposing (RawFile(..))
import Elm.Json.Util exposing (decodeTyped)
import Elm.Syntax.Base exposing (ModuleName, VariablePointer)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix exposing (..)
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Import, Module(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, float, int, lazy, list, map, map2, map3, nullable, string, succeed)
import Json.Decode.Extra exposing (andMap)


rangeField : Decoder (Range -> a) -> Decoder a
rangeField =
    andMap <| field "range" Range.decode


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required s d =
    andMap (field s d)


nameField : Decoder (String -> a) -> Decoder a
nameField =
    required "name" string


{-| Decode a file stored in Json
-}
decode : Decoder RawFile
decode =
    map Raw decodeFile


decodeFile : Decoder File
decodeFile =
    succeed File
        |> required "moduleDefinition" decodeModule
        |> required "imports" (list decodeImport)
        |> required "declarations" (list decodeDeclaration)
        |> required "comments" (list decodeComment)


decodeComment : Decoder (Ranged String)
decodeComment =
    succeed Tuple.pair
        |> required "range" Range.decode
        |> required "text" string


decodeModule : Decoder Module
decodeModule =
    decodeTyped
        [ ( "normal", decodeDefaultModuleData |> map NormalModule )
        , ( "port", decodeDefaultModuleData |> map PortModule )
        , ( "effect", decodeEffectModuleData |> map EffectModule )
        ]


decodeDefaultModuleData : Decoder DefaultModuleData
decodeDefaultModuleData =
    succeed DefaultModuleData
        |> required "moduleName" decodeModuleName
        |> required "exposingList" (decodeExposingList decodeExpose)


decodeEffectModuleData : Decoder EffectModuleData
decodeEffectModuleData =
    succeed EffectModuleData
        |> required "moduleName" decodeModuleName
        |> required "exposingList" (decodeExposingList decodeExpose)
        |> required "command" (nullable string)
        |> required "subscription" (nullable string)


decodeModuleName : Decoder ModuleName
decodeModuleName =
    list string


decodeExpose : Decoder (Ranged TopLevelExpose)
decodeExpose =
    succeed Tuple.pair
        |> required "range" Range.decode
        |> required "topLevel"
            (decodeTyped
                [ ( "infix", succeed InfixExpose |> nameField )
                , ( "function"
                  , succeed FunctionExpose
                        |> nameField
                  )
                , ( "typeOrAlias", succeed TypeOrAliasExpose |> nameField )
                , ( "typeexpose", map TypeExpose decodeExposedType )
                ]
            )


decodeExposedType : Decoder ExposedType
decodeExposedType =
    succeed ExposedType
        |> nameField
        |> required "open" (nullable Range.decode)


decodeValueConstructorExpose : Decoder ValueConstructorExpose
decodeValueConstructorExpose =
    succeed Tuple.pair
        |> rangeField
        |> nameField


decodeExposingList : Decoder (Ranged TopLevelExpose) -> Decoder Exposing
decodeExposingList x =
    lazy
        (\() ->
            decodeTyped
                [ ( "all", Range.decode |> map All )
                , ( "explicit", list x |> map Explicit )
                ]
        )


decodeImport : Decoder Import
decodeImport =
    succeed Import
        |> required "moduleName" decodeModuleName
        |> required "moduleAlias" (nullable decodeModuleName)
        |> required "exposingList" (nullable (decodeExposingList decodeExpose))
        |> rangeField


decodeDeclaration : Decoder (Ranged Declaration)
decodeDeclaration =
    lazy
        (\() ->
            succeed Tuple.pair
                |> rangeField
                |> required "declaration"
                    (decodeTyped
                        [ ( "function", decodeFunction |> map FuncDecl )
                        , ( "typeAlias", decodeTypeAlias |> map AliasDecl )
                        , ( "typedecl", decodeType |> map TypeDecl )
                        , ( "port", decodeSignature |> map PortDeclaration )
                        , ( "infix", Infix.decode |> map InfixDeclaration )
                        , ( "destructuring", map2 Destructuring (field "pattern" decodePattern) (field "expression" decodeExpression) )
                        ]
                    )
        )


decodeType : Decoder Type
decodeType =
    succeed Type
        |> nameField
        |> required "generics" (list string)
        |> required "constructors" (list decodeValueConstructor)


decodeValueConstructor : Decoder ValueConstructor
decodeValueConstructor =
    succeed ValueConstructor
        |> nameField
        |> required "arguments" (list decodeTypeAnnotation)
        |> rangeField


decodeTypeAlias : Decoder TypeAlias
decodeTypeAlias =
    succeed TypeAlias
        |> required "documentation" (nullable decodeDocumentation)
        |> nameField
        |> required "generics" (list string)
        |> required "typeAnnotation" decodeTypeAnnotation


decodeFunction : Decoder Function
decodeFunction =
    lazy
        (\() ->
            succeed Function
                |> required "documentation" (nullable decodeDocumentation)
                |> required "signature" (nullable decodeRangedSignature)
                |> required "declaration" decodeFunctionDeclaration
        )


decodeDocumentation : Decoder Documentation
decodeDocumentation =
    succeed Documentation
        |> required "value" string
        |> rangeField


decodeRangedSignature : Decoder (Ranged FunctionSignature)
decodeRangedSignature =
    succeed Tuple.pair
        |> rangeField
        |> required "signature" decodeSignature


decodeSignature : Decoder FunctionSignature
decodeSignature =
    succeed FunctionSignature
        |> required "name" decodeVariablePointer
        |> required "typeAnnotation" decodeTypeAnnotation


decodeTypeAnnotation : Decoder (Ranged TypeAnnotation)
decodeTypeAnnotation =
    lazy
        (\() ->
            succeed Tuple.pair
                |> required "range" Range.decode
                |> required "typeAnnotation"
                    (decodeTyped
                        [ ( "generic", map GenericType (field "value" string) )
                        , ( "typed"
                          , succeed Typed
                                |> required "moduleName" decodeModuleName
                                |> nameField
                                |> required "args" (list decodeTypeAnnotation)
                          )
                        , ( "unit", succeed Unit )
                        , ( "tupled", map Tupled (field "values" (list decodeTypeAnnotation)) )
                        , ( "function"
                          , map2 FunctionTypeAnnotation
                                (field "left" decodeTypeAnnotation)
                                (field "right" decodeTypeAnnotation)
                          )
                        , ( "record", map Record (field "value" decodeRecordDefinition) )
                        , ( "genericRecord"
                          , succeed GenericRecord
                                |> nameField
                                |> required "values" decodeRecordDefinition
                          )
                        ]
                    )
        )


decodeRecordDefinition : Decoder RecordDefinition
decodeRecordDefinition =
    lazy (\() -> list decodeRecordField)


decodeRecordField : Decoder RecordField
decodeRecordField =
    lazy
        (\() ->
            succeed Tuple.pair
                |> nameField
                |> required "typeAnnotation" decodeTypeAnnotation
        )


decodeFunctionDeclaration : Decoder FunctionDeclaration
decodeFunctionDeclaration =
    lazy
        (\() ->
            succeed FunctionDeclaration
                |> required "name" decodeVariablePointer
                |> required "arguments" (list decodePattern)
                |> required "expression" decodeExpression
        )


decodeVariablePointer : Decoder VariablePointer
decodeVariablePointer =
    succeed VariablePointer
        |> required "value" string
        |> rangeField


decodeChar : Decoder Char
decodeChar =
    string
        |> andThen
            (\s ->
                case String.uncons s of
                    Just ( c, _ ) ->
                        succeed c

                    Nothing ->
                        fail "Not a char"
            )


decodePattern : Decoder (Ranged Pattern)
decodePattern =
    lazy
        (\() ->
            succeed Tuple.pair
                |> rangeField
                |> required "pattern"
                    (decodeTyped
                        [ ( "all", succeed AllPattern )
                        , ( "unit", succeed UnitPattern )
                        , ( "char", field "value" decodeChar |> map CharPattern )
                        , ( "string", field "value" string |> map StringPattern )
                        , ( "hex", int |> map HexPattern )
                        , ( "int", field "value" int |> map IntPattern )
                        , ( "float", field "value" float |> map FloatPattern )
                        , ( "tuple", field "value" (list decodePattern) |> map TuplePattern )
                        , ( "record", field "value" (list decodeVariablePointer) |> map RecordPattern )
                        , ( "uncons", map2 UnConsPattern (field "left" decodePattern) (field "right" decodePattern) )
                        , ( "list", field "value" (list decodePattern) |> map ListPattern )
                        , ( "var", field "value" string |> map VarPattern )
                        , ( "named", map2 NamedPattern (field "qualified" decodeQualifiedNameRef) (field "patterns" (list decodePattern)) )
                        , ( "as", map2 AsPattern (field "pattern" decodePattern) (field "name" decodeVariablePointer) )
                        , ( "parentisized", map ParenthesizedPattern (field "value" decodePattern) )
                        ]
                    )
        )


decodeQualifiedNameRef : Decoder QualifiedNameRef
decodeQualifiedNameRef =
    succeed QualifiedNameRef
        |> required "moduleName" decodeModuleName
        |> nameField


decodeExpression : Decoder (Ranged Expression)
decodeExpression =
    lazy
        (\() ->
            succeed Tuple.pair
                |> rangeField
                |> required "inner" decodeInnerExpression
        )


decodeInnerExpression : Decoder Expression
decodeInnerExpression =
    lazy
        (\() ->
            decodeTyped
                [ ( "unit", succeed UnitExpr )
                , ( "application", list decodeExpression |> map Application )
                , ( "operatorapplication", decodeOperatorApplication )
                , ( "functionOrValue", string |> map FunctionOrValue )
                , ( "ifBlock", map3 IfBlock (field "clause" decodeExpression) (field "then" decodeExpression) (field "else" decodeExpression) )
                , ( "prefixoperator", string |> map PrefixOperator )
                , ( "operator", string |> map Operator )
                , ( "hex", int |> map Hex )
                , ( "integer", int |> map Integer )
                , ( "float", float |> map Floatable )
                , ( "negation", decodeExpression |> map Negation )
                , ( "literal", string |> map Literal )
                , ( "charLiteral", decodeChar |> map CharLiteral )
                , ( "tupled", list decodeExpression |> map TupledExpression )
                , ( "list", list decodeExpression |> map ListExpr )
                , ( "parenthesized", decodeExpression |> map ParenthesizedExpression )
                , ( "let", decodeLetBlock |> map LetExpression )
                , ( "case", decodeCaseBlock |> map CaseExpression )
                , ( "lambda", decodeLambda |> map LambdaExpression )
                , ( "qualified", succeed QualifiedExpr |> required "moduleName" decodeModuleName |> nameField )
                , ( "recordAccess", succeed RecordAccess |> required "expression" decodeExpression |> nameField )
                , ( "recordAccessFunction", string |> map RecordAccessFunction )
                , ( "record", list decodeRecordSetter |> map RecordExpr )
                , ( "recordUpdate", decodeRecordUpdate |> map RecordUpdateExpression )
                , ( "glsl", string |> map GLSLExpression )
                ]
        )


decodeRecordUpdate : Decoder RecordUpdate
decodeRecordUpdate =
    lazy
        (\() ->
            succeed RecordUpdate
                |> nameField
                |> required "updates" (list decodeRecordSetter)
        )


decodeRecordSetter : Decoder RecordSetter
decodeRecordSetter =
    lazy
        (\() ->
            succeed Tuple.pair
                |> required "field" string
                |> required "expression" decodeExpression
        )


decodeLambda : Decoder Lambda
decodeLambda =
    lazy
        (\() ->
            succeed Lambda
                |> required "patterns" (list decodePattern)
                |> required "expression" decodeExpression
        )


decodeCaseBlock : Decoder CaseBlock
decodeCaseBlock =
    lazy
        (\() ->
            succeed CaseBlock
                |> required "expression" decodeExpression
                |> required "cases" (list decodeCase)
        )


decodeCase : Decoder Case
decodeCase =
    lazy
        (\() ->
            succeed Tuple.pair
                |> required "pattern" decodePattern
                |> required "expression" decodeExpression
        )


decodeLetBlock : Decoder LetBlock
decodeLetBlock =
    lazy
        (\() ->
            succeed LetBlock
                |> required "declarations" (list decodeLetDeclaration)
                |> required "expression" decodeExpression
        )


decodeLetDeclaration : Decoder (Ranged LetDeclaration)
decodeLetDeclaration =
    lazy
        (\() ->
            succeed Tuple.pair
                |> rangeField
                |> required "declaration"
                    (decodeTyped
                        [ ( "function", map LetFunction decodeFunction )
                        , ( "destructuring", map2 LetDestructuring (field "pattern" decodePattern) (field "expression" decodeExpression) )
                        ]
                    )
        )


decodeOperatorApplication : Decoder Expression
decodeOperatorApplication =
    lazy
        (\() ->
            succeed OperatorApplication
                |> required "operator" string
                |> required "direction" Infix.decodeDirection
                |> required "left" decodeExpression
                |> required "right" decodeExpression
        )
