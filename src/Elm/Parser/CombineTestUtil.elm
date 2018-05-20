module Elm.Parser.CombineTestUtil exposing (..)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (Range, emptyRange)
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.Type exposing (..)
import Elm.Syntax.TypeAlias exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)


pushIndent : Int -> Parser State b -> Parser State b
pushIndent x p =
    modifyState (Elm.Parser.State.pushColumn (x + 1)) |> Combine.continueWith p


parseFullStringState : State -> String -> Parser State b -> Maybe b
parseFullStringState state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseStateToMaybe : State -> String -> Parser State b -> Maybe ( b, State )
parseStateToMaybe state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( x, r ) ->
            Just ( r, x )

        _ ->
            Nothing


parseFullStringWithNullState : String -> Parser State b -> Maybe b
parseFullStringWithNullState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s |> Debug.log "Result" of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseFullString : String -> Parser () b -> Maybe b
parseFullString s p =
    case Combine.parse (p |> Combine.ignore Combine.end) s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossibleWithState : State -> String -> Parser State b -> Maybe b
parseAsFarAsPossibleWithState state s p =
    case Combine.runParser p state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossible : String -> Parser () b -> Maybe b
parseAsFarAsPossible s p =
    case Combine.parse p s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


emptyRanged : Expression -> Ranged Expression
emptyRanged =
    \a -> ( emptyRange, a )


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


noRangeExposingList : Exposing -> Exposing
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

        HexPattern h ->
            HexPattern h

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


unRanged : (a -> a) -> Ranged a -> Ranged a
unRanged f ( _, a ) =
    ( emptyRange, f a )


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

        TypeExpose { name, open } ->
            TypeExpose (ExposedType name (Maybe.map (always emptyRange) open))
    )


noRangeInfix : Infix -> Infix
noRangeInfix { direction, precedence, operator, function } =
    Infix
        (unRanged identity direction)
        (unRanged identity precedence)
        (unRanged identity operator)
        (unRanged identity function)


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

        AliasDecl aliasDecl ->
            AliasDecl (noRangeTypeAlias aliasDecl)

        InfixDeclaration infixDecl ->
            InfixDeclaration infixDecl


noRangeLetDeclaration : Ranged LetDeclaration -> Ranged LetDeclaration
noRangeLetDeclaration ( r, decl ) =
    ( emptyRange
    , case decl of
        LetFunction function ->
            LetFunction (noRangeFunction function)

        LetDestructuring pattern expression ->
            LetDestructuring (noRangePattern pattern) (noRangeExpression expression)
    )


noRangeTypeAlias : TypeAlias -> TypeAlias
noRangeTypeAlias typeAlias =
    { typeAlias | typeAnnotation = noRangeTypeReference typeAlias.typeAnnotation }


noRangeTypeReference : Ranged TypeAnnotation -> Ranged TypeAnnotation
noRangeTypeReference ( _, typeAnnotation ) =
    ( emptyRange
    , case typeAnnotation of
        GenericType x ->
            GenericType x

        Typed a b c ->
            Typed a b (List.map noRangeTypeReference c)

        Unit ->
            Unit

        Tupled a ->
            Tupled (List.map noRangeTypeReference a)

        Record a ->
            Record (List.map (Tuple.mapSecond noRangeTypeReference) a)

        GenericRecord a b ->
            GenericRecord a (List.map (Tuple.mapSecond noRangeTypeReference) b)

        FunctionTypeAnnotation a b ->
            FunctionTypeAnnotation
                (noRangeTypeReference a)
                (noRangeTypeReference b)
    )


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
        , signature = Maybe.map (unRanged noRangeSignature) f.signature
    }


noRangeSignature : FunctionSignature -> FunctionSignature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation, name = unRange signature.name }


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
