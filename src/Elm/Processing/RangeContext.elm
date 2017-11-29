module Elm.Processing.RangeContext exposing (RangeContext, build, context, postProcess)

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)


type RangeContext
    = Context Int (Dict Int Int)


type alias Patch =
    Range -> Range


context : String -> RangeContext
context input =
    let
        rows =
            String.split "\n" input

        index =
            rows
                |> List.indexedMap (\x y -> ( x, String.length y ))
                |> Dict.fromList
    in
    Context (List.length rows) index


build : RangeContext -> Range -> Range
build (Context rows rangeContext) { start, end } =
    if start.row == 1 then
        { start = { row = start.row - 1, column = start.column }
        , end = realEnd rows rangeContext { row = end.row - 1, column = end.column }
        }
    else
        { start = { row = start.row, column = start.column + 1 }
        , end = realEnd rows rangeContext { row = end.row, column = end.column + 1 }
        }


realEnd : Int -> Dict Int Int -> Location -> Location
realEnd rows d e =
    if e.row > rows then
        { e | row = e.row - 2, column = Dict.get (e.row - 2) d |> Maybe.withDefault 0 }
    else if e.column >= 0 then
        e
    else
        { e | row = e.row - 1, column = Dict.get (e.row - 1) d |> Maybe.withDefault 0 }


postProcess : String -> File -> File
postProcess input f =
    let
        patch =
            build (context input)
    in
    { moduleDefinition = patchModuleDefinition patch f.moduleDefinition
    , imports = List.map (patchImport patch) f.imports
    , declarations = List.map (patchDeclaration patch) f.declarations
    , comments = List.map (patchComment patch) f.comments
    }


patchImport : Patch -> Import -> Import
patchImport patch imp =
    unRange patch { imp | exposingList = Maybe.map (patchExposingList patch patchTopLevelExpose) imp.exposingList }


patchComment : Patch -> ( String, Range ) -> ( String, Range )
patchComment p ( v, r ) =
    ( v, p r )


patchModuleDefinition : Patch -> Module -> Module
patchModuleDefinition p m =
    case m of
        NormalModule defaultModuleData ->
            NormalModule { defaultModuleData | exposingList = patchExposingList p patchTopLevelExpose defaultModuleData.exposingList }

        PortModule defaultModuleData ->
            PortModule { defaultModuleData | exposingList = patchExposingList p patchTopLevelExpose defaultModuleData.exposingList }

        EffectModule effectModuleData ->
            EffectModule { effectModuleData | exposingList = patchExposingList p patchTopLevelExpose effectModuleData.exposingList }


patchExposingList : Patch -> (Patch -> a -> a) -> Exposing a -> Exposing a
patchExposingList p x e =
    case e of
        All range ->
            All (p range)

        Explicit aList ->
            Explicit (List.map (x p) aList)


patchTopLevelExpose : Patch -> TopLevelExpose -> TopLevelExpose
patchTopLevelExpose p e =
    case e of
        InfixExpose s r ->
            InfixExpose s (p r)

        FunctionExpose s r ->
            FunctionExpose s (p r)

        TypeOrAliasExpose s r ->
            TypeOrAliasExpose s (p r)

        TypeExpose et ->
            TypeExpose (patchExposedType p et)


patchExposedType : Patch -> ExposedType -> ExposedType
patchExposedType p et =
    { et | range = p et.range, constructors = Maybe.map (patchExposingList p patchValueConstructorExpose) et.constructors }


patchValueConstructorExpose : Patch -> ValueConstructorExpose -> ValueConstructorExpose
patchValueConstructorExpose p ( v, r ) =
    ( v, p r )


patchPattern : Patch -> Pattern -> Pattern
patchPattern patch p =
    case p of
        QualifiedNamePattern x r ->
            QualifiedNamePattern x (patch r)

        RecordPattern ls r ->
            RecordPattern (List.map (unRange patch) ls) (patch r)

        VarPattern x r ->
            VarPattern x (patch r)

        NamedPattern x y r ->
            NamedPattern x (List.map (patchPattern patch) y) (patch r)

        ParenthesizedPattern x r ->
            ParenthesizedPattern (patchPattern patch x) (patch r)

        AsPattern x y r ->
            AsPattern (patchPattern patch x) (unRange patch y) (patch r)

        UnConsPattern x y r ->
            UnConsPattern (patchPattern patch x) (patchPattern patch y) (patch r)

        CharPattern c r ->
            CharPattern c (patch r)

        StringPattern s r ->
            StringPattern s (patch r)

        FloatPattern f r ->
            FloatPattern f (patch r)

        IntPattern i r ->
            IntPattern i (patch r)

        AllPattern r ->
            AllPattern (patch r)

        UnitPattern r ->
            UnitPattern (patch r)

        ListPattern x r ->
            ListPattern (List.map (patchPattern patch) x) (patch r)

        TuplePattern x r ->
            TuplePattern (List.map (patchPattern patch) x) (patch r)


unRange : Patch -> { a | range : Range } -> { a | range : Range }
unRange patch p =
    { p | range = patch p.range }


patchDeclaration : Patch -> Declaration -> Declaration
patchDeclaration patch decl =
    case decl of
        Destructuring pattern expression ->
            Destructuring
                (patchPattern patch pattern)
                (patchRanged patchExpression patch expression)

        FuncDecl f ->
            FuncDecl <| patchFunction patch f

        TypeDecl d ->
            TypeDecl <| patchTypeDeclaration patch d

        PortDeclaration d ->
            PortDeclaration (patchSignature patch d)

        AliasDecl d ->
            AliasDecl (patchTypeAlias patch d)

        InfixDeclaration d ->
            InfixDeclaration d


patchLetDeclaration : Patch -> LetDeclaration -> LetDeclaration
patchLetDeclaration patch decl =
    case decl of
        LetFunction function ->
            LetFunction (patchFunction patch function)

        LetDestructuring pattern expression ->
            LetDestructuring (patchPattern patch pattern) (patchRanged patchExpression patch expression)


patchTypeAlias : Patch -> TypeAlias -> TypeAlias
patchTypeAlias patch typeAlias =
    unRange patch { typeAlias | typeAnnotation = patchTypeReference patch typeAlias.typeAnnotation }


patchTypeReference : Patch -> TypeAnnotation -> TypeAnnotation
patchTypeReference patch typeAnnotation =
    case typeAnnotation of
        GenericType x r ->
            GenericType x (patch r)

        Typed a b c r ->
            Typed a b (List.map (patchTypeReference patch) c) (patch r)

        Unit r ->
            Unit (patch r)

        Tupled a r ->
            Tupled (List.map (patchTypeReference patch) a) (patch r)

        Record a r ->
            Record (List.map (patchRecordField patch) a) (patch r)

        GenericRecord a b r ->
            GenericRecord a (List.map (patchRecordField patch) b) (patch r)

        FunctionTypeAnnotation a b r ->
            FunctionTypeAnnotation
                (patchTypeReference patch a)
                (patchTypeReference patch b)
                (patch r)


patchRecordField : Patch -> RecordField -> RecordField
patchRecordField patch =
    Tuple.mapSecond (patchTypeReference patch)


patchTypeDeclaration : Patch -> Type -> Type
patchTypeDeclaration patch x =
    { x | constructors = List.map (patchValueConstructor patch) x.constructors }


patchValueConstructor : Patch -> ValueConstructor -> ValueConstructor
patchValueConstructor patch valueConstructor =
    unRange patch { valueConstructor | arguments = List.map (patchTypeReference patch) valueConstructor.arguments }


patchFunction : Patch -> Function -> Function
patchFunction patch f =
    { f
        | declaration = patchFunctionDeclaration patch f.declaration
        , signature = Maybe.map (patchSignature patch) f.signature
    }


patchSignature : Patch -> FunctionSignature -> FunctionSignature
patchSignature patch signature =
    { signature | typeAnnotation = patchTypeReference patch signature.typeAnnotation }
        |> unRange patch


patchFunctionDeclaration : Patch -> FunctionDeclaration -> FunctionDeclaration
patchFunctionDeclaration patch d =
    { d
        | expression = patchRanged patchExpression patch d.expression
        , arguments = List.map (patchPattern patch) d.arguments
        , name = unRange patch d.name
    }


patchRanged : (Patch -> a -> a) -> Patch -> Ranged a -> Ranged a
patchRanged f p ( r, x ) =
    ( p r, f p x )


patchExpression : Patch -> Expression -> Expression
patchExpression patch inner =
    case inner of
        Application xs ->
            Application <| List.map (patchRanged patchExpression patch) xs

        OperatorApplication op dir left right ->
            OperatorApplication op dir (patchRanged patchExpression patch left) (patchRanged patchExpression patch right)

        ListExpr xs ->
            ListExpr <| List.map (patchRanged patchExpression patch) xs

        IfBlock a b c ->
            IfBlock
                (patchRanged patchExpression patch a)
                (patchRanged patchExpression patch b)
                (patchRanged patchExpression patch c)

        RecordExpr fields ->
            RecordExpr <| List.map (Tuple.mapSecond (patchRanged patchExpression patch)) fields

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression = patchRanged patchExpression patch lambda.expression
                    , args = List.map (patchPattern patch) lambda.args
                }

        RecordUpdateExpression update ->
            RecordUpdateExpression { update | updates = List.map (Tuple.mapSecond (patchRanged patchExpression patch)) update.updates }

        CaseExpression { cases, expression } ->
            CaseExpression
                { cases =
                    cases
                        |> List.map (Tuple.mapFirst (patchPattern patch))
                        |> List.map (Tuple.mapSecond (patchRanged patchExpression patch))
                , expression = patchRanged patchExpression patch expression
                }

        LetExpression { declarations, expression } ->
            LetExpression
                { declarations = List.map (patchLetDeclaration patch) declarations
                , expression = patchRanged patchExpression patch expression
                }

        TupledExpression x ->
            TupledExpression <| List.map (patchRanged patchExpression patch) x

        ParenthesizedExpression x ->
            ParenthesizedExpression <| patchRanged patchExpression patch x

        RecordAccess e n ->
            RecordAccess (patchRanged patchExpression patch e) n

        Negation expr ->
            Negation (patchRanged patchExpression patch expr)

        _ ->
            inner
