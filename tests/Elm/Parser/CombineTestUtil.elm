module Elm.Parser.CombineTestUtil exposing (noRangeExpose, noRangeExposingList, noRangeExpression, noRangeImport, noRangeInfix, noRangeInnerExpression, noRangeLetDeclaration, noRangeModule, noRangePattern, noRangeSignature, noRangeTypeAlias, noRangeTypeDeclaration, noRangeTypeReference, parseAsFarAsPossible, parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseStateToMaybe, pushIndent, unRanged)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.Signature exposing (Signature)
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
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
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


noRangeExpression : Node Expression -> Node Expression
noRangeExpression (Node _ inner) =
    Node.empty <| noRangeInnerExpression inner


noRangeModule : Module -> Module
noRangeModule m =
    case m of
        NormalModule n ->
            NormalModule
                { n
                    | moduleName = unRange n.moduleName
                    , exposingList = unRanged noRangeExposingList n.exposingList
                }

        PortModule n ->
            PortModule
                { n
                    | moduleName = unRange n.moduleName
                    , exposingList = unRanged noRangeExposingList n.exposingList
                }

        EffectModule n ->
            EffectModule
                { n
                    | moduleName = unRange n.moduleName
                    , exposingList = unRanged noRangeExposingList n.exposingList
                    , command = Maybe.map unRange n.command
                    , subscription = Maybe.map unRange n.subscription
                }


noRangeImport : Import -> Import
noRangeImport imp =
    { imp
        | exposingList = Maybe.map (unRanged noRangeExposingList) imp.exposingList
        , moduleName = unRange imp.moduleName
        , moduleAlias = Maybe.map unRange imp.moduleAlias
    }


noRangeExposingList : Exposing -> Exposing
noRangeExposingList x =
    case x of
        All _ ->
            All empty

        Explicit head rest ->
            Explicit (noRangeExpose head) (List.map noRangeExpose rest)


noRangePattern : Node Pattern -> Node Pattern
noRangePattern (Node _ p) =
    Node.empty <|
        case p of
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


noRangeDestructurePattern : Node DestructurePattern -> Node DestructurePattern
noRangeDestructurePattern (Node _ p) =
    Node.empty <|
        case p of
            RecordPattern_ ls ->
                RecordPattern_ (List.map unRange ls)

            VarPattern_ x ->
                VarPattern_ x

            NamedPattern_ x y ->
                NamedPattern_ x (List.map noRangeDestructurePattern y)

            ParenthesizedPattern_ x ->
                ParenthesizedPattern_ (noRangeDestructurePattern x)

            AsPattern_ x y ->
                AsPattern_ (noRangeDestructurePattern x) (unRange y)

            AllPattern_ ->
                AllPattern_

            UnitPattern_ ->
                UnitPattern_

            TuplePattern_ x ->
                TuplePattern_ (List.map noRangeDestructurePattern x)


unRange : Node a -> Node a
unRange n =
    unRanged identity n


unRanged : (a -> a) -> Node a -> Node a
unRanged f (Node _ a) =
    Node.empty <| f a


noRangeExpose : Node TopLevelExpose -> Node TopLevelExpose
noRangeExpose (Node _ l) =
    Node.empty <|
        case l of
            InfixExpose s ->
                InfixExpose s

            FunctionExpose s ->
                FunctionExpose s

            TypeOrAliasExpose s ->
                TypeOrAliasExpose s

            TypeExpose { name, open } ->
                TypeExpose (ExposedType name (Maybe.map (always empty) open))


noRangeInfix : Infix -> Infix
noRangeInfix { direction, precedence, operator, function } =
    Infix
        (unRange direction)
        (unRange precedence)
        (unRange operator)
        (unRange function)


noRangeLetDeclaration : Node LetDeclaration -> Node LetDeclaration
noRangeLetDeclaration (Node _ decl) =
    Node.empty <|
        case decl of
            LetFunction function ->
                LetFunction (noRangeFunction function)

            LetDestructuring pattern expression ->
                LetDestructuring (noRangeDestructurePattern pattern) (noRangeExpression expression)


noRangeTypeAlias : TypeAlias -> TypeAlias
noRangeTypeAlias typeAlias =
    { typeAlias
        | generics = List.map unRange typeAlias.generics
        , name = unRange typeAlias.name
        , documentation = Maybe.map unRange typeAlias.documentation
        , typeAnnotation = noRangeTypeReference typeAlias.typeAnnotation
    }


noRangeRecordField : RecordField -> RecordField
noRangeRecordField ( a, b ) =
    ( unRange a, noRangeTypeReference b )


noRangeRecordDefinition : RecordDefinition -> RecordDefinition
noRangeRecordDefinition =
    List.map (unRanged noRangeRecordField)


noRangeTypeReference : Node TypeAnnotation -> Node TypeAnnotation
noRangeTypeReference (Node _ typeAnnotation) =
    Node.empty <|
        case typeAnnotation of
            Var x ->
                Var x

            Elm.Syntax.TypeAnnotation.Type (Node _ ( a, b )) c ->
                Elm.Syntax.TypeAnnotation.Type (Node.empty ( a, b )) (List.map noRangeTypeReference c)

            Tuple a ->
                Tuple (List.map noRangeTypeReference a)

            Record a ->
                Record (List.map (unRanged noRangeRecordField) a)

            GenericRecord a b ->
                GenericRecord (unRanged identity a) (unRanged noRangeRecordDefinition b)

            FunctionTypeAnnotation a b ->
                FunctionTypeAnnotation
                    (noRangeTypeReference a)
                    (noRangeTypeReference b)


noRangeTypeDeclaration : Type -> Type
noRangeTypeDeclaration x =
    { x
        | firstConstructor = unRanged noRangeValueConstructor x.firstConstructor
        , restOfConstructors = List.map (unRanged noRangeValueConstructor) x.restOfConstructors
        , generics = List.map unRange x.generics
        , name = unRange x.name
    }


noRangeValueConstructor : ValueConstructor -> ValueConstructor
noRangeValueConstructor valueConstructor =
    { valueConstructor | arguments = List.map noRangeTypeReference valueConstructor.arguments, name = unRange valueConstructor.name }


noRangeFunction : Function -> Function
noRangeFunction f =
    { f
        | declaration = unRanged noRangeFunctionImplementation f.declaration
        , signature = Maybe.map (unRanged noRangeSignature) f.signature
    }


noRangeSignature : Signature -> Signature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation, name = unRange signature.name }


noRangeFunctionImplementation : FunctionImplementation -> FunctionImplementation
noRangeFunctionImplementation d =
    { d
        | expression = noRangeExpression d.expression
        , arguments = List.map noRangeDestructurePattern d.arguments
        , name = unRange d.name
    }


noRangeRecordSetter : RecordSetter -> RecordSetter
noRangeRecordSetter ( a, b ) =
    ( unRange a, unRanged noRangeInnerExpression b )


noRangeInnerExpression : Expression -> Expression
noRangeInnerExpression inner =
    case inner of
        Application head xs ->
            Application (noRangeExpression head) (List.map noRangeExpression xs)

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
            RecordExpr <| List.map (unRanged noRangeRecordSetter) fields

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression = noRangeExpression lambda.expression
                    , firstArg = noRangeDestructurePattern lambda.firstArg
                    , restOfArgs = List.map noRangeDestructurePattern lambda.restOfArgs
                }

        RecordUpdateExpression name firstUpdate updates ->
            RecordUpdateExpression
                (unRanged identity name)
                (unRanged noRangeRecordSetter firstUpdate)
                (List.map (unRanged noRangeRecordSetter) updates)

        CaseExpression { firstCase, restOfCases, expression } ->
            CaseExpression
                { firstCase = Tuple.mapBoth noRangePattern noRangeExpression firstCase
                , restOfCases = List.map (Tuple.mapBoth noRangePattern noRangeExpression) restOfCases
                , expression = noRangeExpression expression
                }

        LetExpression { declarations, expression } ->
            LetExpression
                { declarations = List.map noRangeLetDeclaration declarations
                , expression = noRangeExpression expression
                }

        TupleExpression x ->
            TupleExpression <| List.map noRangeExpression x

        RecordAccess e n ->
            RecordAccess (noRangeExpression e) (unRange n)

        Negation expr ->
            Negation (noRangeExpression expr)

        _ ->
            inner
