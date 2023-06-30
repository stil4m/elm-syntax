module Elm.Parser.CombineTestUtil exposing (noRangeExpose, noRangeExposingList, noRangeExpression, noRangeImport, noRangeInfix, noRangeInnerExpression, noRangeLetDeclaration, noRangeModule, noRangePattern, noRangeSignature, noRangeTypeAlias, noRangeTypeDeclaration, noRangeTypeReference, parseAsFarAsPossible, parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseStateToMaybe, pushIndent, unRanged)

import Combine
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


pushIndent : Int -> Combine.Parser State b -> Combine.Parser State b
pushIndent x p =
    Combine.modifyState (Elm.Parser.State.pushColumn (x + 1)) |> Combine.continueWith p


parseFullStringState : State -> String -> Combine.Parser State b -> Maybe b
parseFullStringState state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseStateToMaybe : State -> String -> Combine.Parser State b -> Maybe ( b, State )
parseStateToMaybe state s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) state s of
        Ok ( x, r ) ->
            Just ( r, x )

        _ ->
            Nothing


parseFullStringWithNullState : String -> Combine.Parser State b -> Maybe b
parseFullStringWithNullState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) emptyState s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseFullString : String -> Combine.Parser () b -> Maybe b
parseFullString s p =
    case Combine.parse (p |> Combine.ignore Combine.end) s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossibleWithState : State -> String -> Combine.Parser State b -> Maybe b
parseAsFarAsPossibleWithState state s p =
    case Combine.runParser p state s of
        Ok ( _, r ) ->
            Just r

        _ ->
            Nothing


parseAsFarAsPossible : String -> Combine.Parser () b -> Maybe b
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
        Module.NormalModule n ->
            Module.NormalModule
                { n
                    | moduleName = unRange n.moduleName
                    , exposingList = unRanged noRangeExposingList n.exposingList
                }

        Module.PortModule n ->
            Module.PortModule
                { n
                    | moduleName = unRange n.moduleName
                    , exposingList = unRanged noRangeExposingList n.exposingList
                }

        Module.EffectModule n ->
            Module.EffectModule
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
        Exposing.All _ ->
            Exposing.All empty

        Exposing.Explicit head rest ->
            Exposing.Explicit (noRangeExpose head) (List.map noRangeExpose rest)


noRangePattern : Node Pattern -> Node Pattern
noRangePattern (Node _ p) =
    Node.empty <|
        case p of
            Pattern.RecordPattern ls ->
                Pattern.RecordPattern (List.map unRange ls)

            Pattern.VarPattern x ->
                Pattern.VarPattern x

            Pattern.NamedPattern x y ->
                Pattern.NamedPattern x (List.map noRangePattern y)

            Pattern.ParenthesizedPattern x ->
                Pattern.ParenthesizedPattern (noRangePattern x)

            Pattern.AsPattern x y ->
                Pattern.AsPattern (noRangePattern x) (unRange y)

            Pattern.UnConsPattern x y ->
                Pattern.UnConsPattern (noRangePattern x) (noRangePattern y)

            Pattern.CharPattern c ->
                Pattern.CharPattern c

            Pattern.StringPattern s ->
                Pattern.StringPattern s

            Pattern.HexPattern h ->
                Pattern.HexPattern h

            Pattern.IntPattern i ->
                Pattern.IntPattern i

            Pattern.AllPattern ->
                Pattern.AllPattern

            Pattern.UnitPattern ->
                Pattern.UnitPattern

            Pattern.ListPattern x ->
                Pattern.ListPattern (List.map noRangePattern x)

            Pattern.TuplePattern x ->
                Pattern.TuplePattern (List.map noRangePattern x)


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


noRangeExpose : Node Exposing.TopLevelExpose -> Node Exposing.TopLevelExpose
noRangeExpose (Node _ l) =
    Node.empty <|
        case l of
            Exposing.InfixExpose s ->
                Exposing.InfixExpose s

            Exposing.FunctionExpose s ->
                Exposing.FunctionExpose s

            Exposing.TypeOrAliasExpose s ->
                Exposing.TypeOrAliasExpose s

            Exposing.TypeExpose { name, open } ->
                Exposing.TypeExpose (Exposing.ExposedType name (Maybe.map (always empty) open))


noRangeInfix : Infix -> Infix
noRangeInfix { direction, precedence, operator, function } =
    Infix
        (unRange direction)
        (unRange precedence)
        (unRange operator)
        (unRange function)


noRangeLetDeclaration : Node Expression.LetDeclaration -> Node Expression.LetDeclaration
noRangeLetDeclaration (Node _ decl) =
    Node.empty <|
        case decl of
            Expression.LetFunction function ->
                Expression.LetFunction (noRangeFunction function)

            Expression.LetDestructuring pattern expression ->
                Expression.LetDestructuring (noRangeDestructurePattern pattern) (noRangeExpression expression)


noRangeTypeAlias : TypeAlias -> TypeAlias
noRangeTypeAlias typeAlias =
    { typeAlias
        | generics = List.map unRange typeAlias.generics
        , name = unRange typeAlias.name
        , documentation = Maybe.map unRange typeAlias.documentation
        , typeAnnotation = noRangeTypeReference typeAlias.typeAnnotation
    }


noRangeRecordField : TypeAnnotation.RecordField -> TypeAnnotation.RecordField
noRangeRecordField ( a, b ) =
    ( unRange a, noRangeTypeReference b )


noRangeRecordDefinition : TypeAnnotation.RecordDefinition -> TypeAnnotation.RecordDefinition
noRangeRecordDefinition =
    List.map (unRanged noRangeRecordField)


noRangeTypeReference : Node TypeAnnotation -> Node TypeAnnotation
noRangeTypeReference (Node _ typeAnnotation) =
    Node.empty <|
        case typeAnnotation of
            TypeAnnotation.Var x ->
                TypeAnnotation.Var x

            TypeAnnotation.Type (Node _ ( a, b )) c ->
                TypeAnnotation.Type (Node.empty ( a, b )) (List.map noRangeTypeReference c)

            TypeAnnotation.Tuple a ->
                TypeAnnotation.Tuple (List.map noRangeTypeReference a)

            TypeAnnotation.Record a ->
                TypeAnnotation.Record (List.map (unRanged noRangeRecordField) a)

            TypeAnnotation.GenericRecord a b ->
                TypeAnnotation.GenericRecord (unRanged identity a) (unRanged noRangeRecordDefinition b)

            TypeAnnotation.FunctionTypeAnnotation a b ->
                TypeAnnotation.FunctionTypeAnnotation
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


noRangeValueConstructor : Type.ValueConstructor -> Type.ValueConstructor
noRangeValueConstructor valueConstructor =
    { valueConstructor | arguments = List.map noRangeTypeReference valueConstructor.arguments, name = unRange valueConstructor.name }


noRangeFunction : Expression.Function -> Expression.Function
noRangeFunction f =
    { f
        | declaration = unRanged noRangeFunctionImplementation f.declaration
        , signature = Maybe.map (unRanged noRangeSignature) f.signature
    }


noRangeSignature : Signature -> Signature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation, name = unRange signature.name }


noRangeFunctionImplementation : Expression.FunctionImplementation -> Expression.FunctionImplementation
noRangeFunctionImplementation d =
    { d
        | expression = noRangeExpression d.expression
        , arguments = List.map noRangeDestructurePattern d.arguments
        , name = unRange d.name
    }


noRangeRecordSetter : Expression.RecordSetter -> Expression.RecordSetter
noRangeRecordSetter ( a, b ) =
    ( unRange a, unRanged noRangeInnerExpression b )


noRangeInnerExpression : Expression -> Expression
noRangeInnerExpression inner =
    case inner of
        Expression.Application head xs ->
            Expression.Application (noRangeExpression head) (List.map noRangeExpression xs)

        Expression.Operation op dir left right ->
            Expression.Operation op dir (noRangeExpression left) (noRangeExpression right)

        Expression.ListLiteral xs ->
            Expression.ListLiteral <| List.map noRangeExpression xs

        Expression.If a b c ->
            Expression.If
                (noRangeExpression a)
                (noRangeExpression b)
                (noRangeExpression c)

        Expression.Record fields ->
            Expression.Record <| List.map (unRanged noRangeRecordSetter) fields

        Expression.LambdaExpression lambda ->
            Expression.LambdaExpression
                { lambda
                    | expression = noRangeExpression lambda.expression
                    , firstArg = noRangeDestructurePattern lambda.firstArg
                    , restOfArgs = List.map noRangeDestructurePattern lambda.restOfArgs
                }

        Expression.RecordUpdate name firstUpdate updates ->
            Expression.RecordUpdate
                (unRanged identity name)
                (unRanged noRangeRecordSetter firstUpdate)
                (List.map (unRanged noRangeRecordSetter) updates)

        Expression.CaseExpression { firstCase, restOfCases, expression } ->
            Expression.CaseExpression
                { firstCase = Tuple.mapBoth noRangePattern noRangeExpression firstCase
                , restOfCases = List.map (Tuple.mapBoth noRangePattern noRangeExpression) restOfCases
                , expression = noRangeExpression expression
                }

        Expression.LetExpression { declarations, expression } ->
            Expression.LetExpression
                { declarations = List.map noRangeLetDeclaration declarations
                , expression = noRangeExpression expression
                }

        Expression.TupleExpression x ->
            Expression.TupleExpression <| List.map noRangeExpression x

        Expression.RecordAccess e n ->
            Expression.RecordAccess (noRangeExpression e) (unRange n)

        Expression.Negation expr ->
            Expression.Negation (noRangeExpression expr)

        _ ->
            inner
