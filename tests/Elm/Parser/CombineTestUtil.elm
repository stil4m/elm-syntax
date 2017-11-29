module Elm.Parser.CombineTestUtil exposing (..)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Range, emptyRange)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)


pushIndent : Int -> Parser State b -> Parser State b
pushIndent x p =
    modifyState (Elm.Parser.State.pushIndent x) *> p


parseFullStringState : State -> String -> Parser State b -> Maybe b
parseFullStringState state s p =
    case Combine.runParser (p <* Combine.end) state s of
        Ok ( _, _, r ) ->
            Just r

        _ ->
            Nothing


parseStateToMaybe : State -> String -> Parser State b -> Maybe ( b, State )
parseStateToMaybe state s p =
    case Combine.runParser (p <* Combine.end) state s of
        Ok ( x, _, r ) ->
            Just ( r, x )

        _ ->
            Nothing


parseFullStringWithNullState : String -> Parser State b -> Maybe b
parseFullStringWithNullState s p =
    case Combine.runParser (p <* Combine.end) emptyState s of
        Ok ( _, _, r ) ->
            Just r

        _ ->
            Nothing


parseFullString : String -> Parser () b -> Maybe b
parseFullString s p =
    case Combine.parse (p <* Combine.end) s of
        Ok ( _, _, r ) ->
            Just r

        _ ->
            Nothing


emptyRanged : Expression -> Ranged Expression
emptyRanged =
    (,) emptyRange


noRangeExpression : Ranged Expression -> Ranged Expression
noRangeExpression ( _, inner ) =
    ( emptyRange, noRangeInnerExpression inner )


noRangeFile : File -> File
noRangeFile file =
    { file
        | moduleDefinition = noRangeModule file.moduleDefinition
        , imports = List.map noRangeImport file.imports
    }


noRangeModule : Module -> Module
noRangeModule m =
    case m of
        NormalModule n ->
            NormalModule { n | exposingList = noRangeExposingList n.exposingList }

        PortModule n ->
            PortModule { n | exposingList = noRangeExposingList n.exposingList }

        EffectModule n ->
            EffectModule { n | exposingList = noRangeExposingList n.exposingList }


noRangeImport : Import -> Import
noRangeImport imp =
    { imp
        | range = emptyRange
        , exposingList = Maybe.map noRangeExposingList imp.exposingList
    }


noRangeExposingList : Exposing (Ranged TopLevelExpose) -> Exposing (Ranged TopLevelExpose)
noRangeExposingList x =
    case x of
        All r ->
            All emptyRange

        Explicit list ->
            list
                |> List.map noRangeExpose
                |> Explicit


noRangePattern : Ranged Pattern -> Ranged Pattern
noRangePattern ( r, p ) =
    ( emptyRange
    , case p of
        QualifiedNamePattern x ->
            QualifiedNamePattern x

        RecordPattern ls ->
            RecordPattern (List.map unRange ls)

        VarPattern x ->
            VarPattern x

        NamedPattern x y ->
            NamedPattern x (List.map noRangePattern y)

        ParenthesizedPattern x ->
            ParenthesizedPattern (noRangePattern x)

        AsPattern x y ->
            AsPattern (noRangePattern x) (unRange y)

        UnConsPattern x y ->
            UnConsPattern (noRangePattern x) (noRangePattern y)

        CharPattern c ->
            CharPattern c

        StringPattern s ->
            StringPattern s

        FloatPattern f ->
            FloatPattern f

        IntPattern i ->
            IntPattern i

        AllPattern ->
            AllPattern

        UnitPattern ->
            UnitPattern

        ListPattern x ->
            ListPattern (List.map noRangePattern x)

        TuplePattern x ->
            TuplePattern (List.map noRangePattern x)
    )


unRange : { a | range : Range } -> { a | range : Range }
unRange p =
    { p | range = emptyRange }


noRangeExpose : Ranged TopLevelExpose -> Ranged TopLevelExpose
noRangeExpose ( _, l ) =
    ( emptyRange
    , case l of
        InfixExpose s ->
            InfixExpose s

        FunctionExpose s ->
            FunctionExpose s

        TypeOrAliasExpose s ->
            TypeOrAliasExpose s

        TypeExpose { name, constructors } ->
            let
                newT =
                    case constructors of
                        Nothing ->
                            Nothing

                        Just (All r) ->
                            Just <| All emptyRange

                        Just (Explicit list) ->
                            Just <| Explicit <| List.map (Tuple.mapFirst (always emptyRange)) list
            in
            TypeExpose (ExposedType name newT)
    )


noRangeDeclaration : Declaration -> Declaration
noRangeDeclaration decl =
    case decl of
        Destructuring pattern expression ->
            Destructuring
                (noRangePattern pattern)
                (noRangeExpression expression)

        FuncDecl f ->
            FuncDecl <| noRangeFunction f

        TypeDecl d ->
            TypeDecl <| noRangeTypeDeclaration d

        PortDeclaration d ->
            PortDeclaration (noRangeSignature d)

        _ ->
            decl


noRangeLetDeclaration : LetDeclaration -> LetDeclaration
noRangeLetDeclaration decl =
    case decl of
        LetFunction function ->
            LetFunction (noRangeFunction function)

        LetDestructuring pattern expression ->
            LetDestructuring (noRangePattern pattern) (noRangeExpression expression)


noRangeTypeAlias : TypeAlias -> TypeAlias
noRangeTypeAlias typeAlias =
    unRange { typeAlias | typeAnnotation = noRangeTypeReference typeAlias.typeAnnotation }


noRangeTypeReference : TypeAnnotation -> TypeAnnotation
noRangeTypeReference typeAnnotation =
    case typeAnnotation of
        GenericType x _ ->
            GenericType x emptyRange

        Typed a b c _ ->
            Typed a b (List.map noRangeTypeReference c) emptyRange

        Unit _ ->
            Unit emptyRange

        Tupled a _ ->
            Tupled (List.map noRangeTypeReference a) emptyRange

        Record a _ ->
            Record (List.map noRangeRecordField a) emptyRange

        GenericRecord a b _ ->
            GenericRecord a (List.map noRangeRecordField b) emptyRange

        FunctionTypeAnnotation a b _ ->
            FunctionTypeAnnotation
                (noRangeTypeReference a)
                (noRangeTypeReference b)
                emptyRange


noRangeRecordField : RecordField -> RecordField
noRangeRecordField =
    Tuple.mapSecond noRangeTypeReference


noRangeTypeDeclaration : Type -> Type
noRangeTypeDeclaration x =
    { x | constructors = List.map noRangeValueConstructor x.constructors }


noRangeValueConstructor : ValueConstructor -> ValueConstructor
noRangeValueConstructor valueConstructor =
    unRange { valueConstructor | arguments = List.map noRangeTypeReference valueConstructor.arguments }


noRangeFunction : Function -> Function
noRangeFunction f =
    { f
        | declaration = noRangeFunctionDeclaration f.declaration
        , signature = Maybe.map noRangeSignature f.signature
    }


noRangeSignature : FunctionSignature -> FunctionSignature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation }
        |> unRange


noRangeFunctionDeclaration : FunctionDeclaration -> FunctionDeclaration
noRangeFunctionDeclaration d =
    { d
        | expression = noRangeExpression d.expression
        , arguments = List.map noRangePattern d.arguments
        , name = unRange d.name
    }


noRangeInnerExpression : Expression -> Expression
noRangeInnerExpression inner =
    case inner of
        Application xs ->
            Application <| List.map noRangeExpression xs

        OperatorApplication op dir left right ->
            OperatorApplication op dir (noRangeExpression left) (noRangeExpression right)

        ListExpr xs ->
            ListExpr <| List.map noRangeExpression xs

        IfBlock a b c ->
            IfBlock
                (noRangeExpression a)
                (noRangeExpression b)
                (noRangeExpression c)

        RecordExpr fields ->
            RecordExpr <| List.map (Tuple.mapSecond noRangeExpression) fields

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression = noRangeExpression lambda.expression
                    , args = List.map noRangePattern lambda.args
                }

        RecordUpdateExpression update ->
            RecordUpdateExpression { update | updates = List.map (Tuple.mapSecond noRangeExpression) update.updates }

        CaseExpression { cases, expression } ->
            CaseExpression
                { cases =
                    cases
                        |> List.map (Tuple.mapFirst noRangePattern)
                        |> List.map (Tuple.mapSecond noRangeExpression)
                , expression = noRangeExpression expression
                }

        LetExpression { declarations, expression } ->
            LetExpression
                { declarations = List.map noRangeLetDeclaration declarations
                , expression = noRangeExpression expression
                }

        TupledExpression x ->
            TupledExpression <| List.map noRangeExpression x

        ParenthesizedExpression x ->
            ParenthesizedExpression <| noRangeExpression x

        RecordAccess e n ->
            RecordAccess (noRangeExpression e) n

        Negation expr ->
            Negation (noRangeExpression expr)

        _ ->
            inner
