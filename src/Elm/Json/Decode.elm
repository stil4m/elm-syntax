module Elm.Json.Decode exposing (decode)

{-|


# Elm.Json.Decode

Decoding Elm Code from Json

@docs decode

-}

import Elm.Internal.RawFile exposing (RawFile(Raw))
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
import Json.Decode.Extra exposing ((|:))


rangeField : Decoder Range
rangeField =
    field "range" Range.decode


nameField : Decoder String
nameField =
    field "name" string


{-| Decode a file stored in Json
-}
decode : Decoder RawFile
decode =
    map Raw decodeFile


decodeFile : Decoder File
decodeFile =
    succeed File
        |: field "moduleDefinition" decodeModule
        |: field "imports" (list decodeImport)
        |: field "declarations" (list decodeDeclaration)
        |: field "comments" (list decodeComment)


decodeComment : Decoder (Ranged String)
decodeComment =
    succeed (,)
        |: field "range" Range.decode
        |: field "text" string


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
        |: field "moduleName" decodeModuleName
        |: field "exposingList" (decodeExposingList decodeExpose)


decodeEffectModuleData : Decoder EffectModuleData
decodeEffectModuleData =
    succeed EffectModuleData
        |: field "moduleName" decodeModuleName
        |: field "exposingList" (decodeExposingList decodeExpose)
        |: field "command" (nullable string)
        |: field "subscription" (nullable string)


decodeModuleName : Decoder ModuleName
decodeModuleName =
    list string


decodeExpose : Decoder (Ranged TopLevelExpose)
decodeExpose =
    succeed (,)
        |: field "range" Range.decode
        |: field "topLevel"
            (decodeTyped
                [ ( "infix", map InfixExpose nameField )
                , ( "function", map FunctionExpose nameField )
                , ( "typeOrAlias", map TypeOrAliasExpose nameField )
                , ( "typeexpose", map TypeExpose decodeExposedType )
                ]
            )


decodeExposedType : Decoder ExposedType
decodeExposedType =
    succeed ExposedType
        |: nameField
        |: field "inner" (nullable (decodeExposingList decodeValueConstructorExpose))


decodeValueConstructorExpose : Decoder ValueConstructorExpose
decodeValueConstructorExpose =
    succeed (,)
        |: rangeField
        |: nameField


decodeExposingList : Decoder a -> Decoder (Exposing a)
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
        |: field "moduleName" decodeModuleName
        |: field "moduleAlias" (nullable decodeModuleName)
        |: field "exposingList" (nullable (decodeExposingList decodeExpose))
        |: rangeField


decodeDeclaration : Decoder (Ranged Declaration)
decodeDeclaration =
    lazy
        (\() ->
            succeed (,)
                |: rangeField
                |: field "declaration"
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
        |: nameField
        |: field "generics" (list string)
        |: field "constructors" (list decodeValueConstructor)


decodeValueConstructor : Decoder ValueConstructor
decodeValueConstructor =
    succeed ValueConstructor
        |: nameField
        |: field "arguments" (list decodeTypeAnnotation)
        |: rangeField


decodeTypeAlias : Decoder TypeAlias
decodeTypeAlias =
    succeed TypeAlias
        |: field "documentation" (nullable decodeDocumentation)
        |: nameField
        |: field "generics" (list string)
        |: field "typeAnnotation" decodeTypeAnnotation


decodeFunction : Decoder Function
decodeFunction =
    lazy
        (\() ->
            succeed Function
                |: field "documentation" (nullable decodeDocumentation)
                |: field "signature" (nullable decodeRangedSignature)
                |: field "declaration" decodeFunctionDeclaration
        )


decodeDocumentation : Decoder Documentation
decodeDocumentation =
    succeed Documentation
        |: field "value" string
        |: rangeField


decodeRangedSignature : Decoder (Ranged FunctionSignature)
decodeRangedSignature =
    succeed (,)
        |: rangeField
        |: field "signature" decodeSignature


decodeSignature : Decoder FunctionSignature
decodeSignature =
    succeed FunctionSignature
        |: field "operatorDefinition" bool
        |: field "name" decodeVariablePointer
        |: field "typeAnnotation" decodeTypeAnnotation


decodeTypeAnnotation : Decoder (Ranged TypeAnnotation)
decodeTypeAnnotation =
    lazy
        (\() ->
            succeed (,)
                |: field "range" Range.decode
                |: field "typeAnnotation"
                    (decodeTyped
                        [ ( "generic", map GenericType (field "value" string) )
                        , ( "typed"
                          , map3 Typed
                                (field "moduleName" decodeModuleName)
                                nameField
                                (field "args" <| list decodeTypeAnnotation)
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
                          , map2 GenericRecord
                                nameField
                                (field "values" decodeRecordDefinition)
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
            succeed (,)
                |: nameField
                |: field "typeAnnotation" decodeTypeAnnotation
        )


decodeFunctionDeclaration : Decoder FunctionDeclaration
decodeFunctionDeclaration =
    lazy
        (\() ->
            succeed FunctionDeclaration
                |: field "operatorDefinition" bool
                |: field "name" decodeVariablePointer
                |: field "arguments" (list decodePattern)
                |: field "expression" decodeExpression
        )


decodeVariablePointer : Decoder VariablePointer
decodeVariablePointer =
    succeed VariablePointer
        |: field "value" string
        |: rangeField


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
            succeed (,)
                |: rangeField
                |: field "pattern"
                    (decodeTyped
                        [ ( "all", succeed AllPattern )
                        , ( "unit", succeed UnitPattern )
                        , ( "char", field "value" decodeChar |> map CharPattern )
                        , ( "string", field "value" string |> map StringPattern )
                        , ( "int", field "value" int |> map IntPattern )
                        , ( "hex", field "value" int |> map HexPattern )
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
        |: field "moduleName" decodeModuleName
        |: nameField


decodeExpression : Decoder (Ranged Expression)
decodeExpression =
    lazy
        (\() ->
            succeed (,)
                |: rangeField
                |: field "inner" decodeInnerExpression
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
                , ( "integer", int |> map Integer )
                , ( "hex", int |> map Hex )
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
                , ( "qualified", map2 QualifiedExpr (field "moduleName" decodeModuleName) nameField )
                , ( "recordAccess", map2 RecordAccess (field "expression" decodeExpression) nameField )
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
                |: nameField
                |: field "updates" (list decodeRecordSetter)
        )


decodeRecordSetter : Decoder RecordSetter
decodeRecordSetter =
    lazy
        (\() ->
            succeed (,)
                |: field "field" string
                |: field "expression" decodeExpression
        )


decodeLambda : Decoder Lambda
decodeLambda =
    lazy
        (\() ->
            succeed Lambda
                |: field "patterns" (list decodePattern)
                |: field "expression" decodeExpression
        )


decodeCaseBlock : Decoder CaseBlock
decodeCaseBlock =
    lazy
        (\() ->
            succeed CaseBlock
                |: field "expression" decodeExpression
                |: field "cases" (list decodeCase)
        )


decodeCase : Decoder Case
decodeCase =
    lazy
        (\() ->
            succeed (,)
                |: field "pattern" decodePattern
                |: field "expression" decodeExpression
        )


decodeLetBlock : Decoder LetBlock
decodeLetBlock =
    lazy
        (\() ->
            succeed LetBlock
                |: field "declarations" (list decodeLetDeclaration)
                |: field "expression" decodeExpression
        )


decodeLetDeclaration : Decoder (Ranged LetDeclaration)
decodeLetDeclaration =
    lazy
        (\() ->
            succeed (,)
                |: rangeField
                |: field "declaration"
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
                |: field "operator" string
                |: field "direction" Infix.decodeDirection
                |: field "left" decodeExpression
                |: field "right" decodeExpression
        )
