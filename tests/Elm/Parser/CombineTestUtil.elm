module Elm.Parser.CombineTestUtil exposing (emptyRanged, noRangeDeclaration, noRangeExpose, noRangeExposingList, noRangeExpression, noRangeImport, noRangeInfix, noRangeInnerExpression, noRangeLetDeclaration, noRangeModule, noRangePattern, noRangeSignature, noRangeTypeAlias, noRangeTypeDeclaration, noRangeTypeReference, parseAsFarAsPossible, parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseStateToMaybe, pushIndent, unRanged)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
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


emptyRanged : Expression -> Node Expression
emptyRanged =
    Node emptyRange


noRangeExpression : Node Expression -> Node Expression
noRangeExpression (Node _ inner) =
    Node emptyRange <| noRangeInnerExpression inner


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
            All emptyRange

        Explicit list ->
            list
                |> List.map noRangeExpose
                |> Explicit


noRangePattern : Node Pattern -> Node Pattern
noRangePattern (Node _ p) =
    Node emptyRange <|
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


unRange : Node a -> Node a
unRange n =
    unRanged identity n


unRanged : (a -> a) -> Node a -> Node a
unRanged f (Node _ a) =
    Node emptyRange <| f a


noRangeExpose : Node TopLevelExpose -> Node TopLevelExpose
noRangeExpose (Node _ l) =
    Node emptyRange <|
        case l of
            InfixExpose s ->
                InfixExpose s

            FunctionExpose s ->
                FunctionExpose s

            TypeOrAliasExpose s ->
                TypeOrAliasExpose s

            TypeExpose { name, open } ->
                TypeExpose (ExposedType name (Maybe.map (always emptyRange) open))


noRangeInfix : Infix -> Infix
noRangeInfix { direction, precedence, operator, function } =
    Infix
        (unRange direction)
        (unRange precedence)
        (unRange operator)
        (unRange function)


noRangeDeclaration : Declaration -> Declaration
noRangeDeclaration decl =
    case decl of
        Destructuring pattern expression ->
            Destructuring
                (noRangePattern pattern)
                (noRangeExpression expression)

        FunctionDeclaration f ->
            FunctionDeclaration <| noRangeFunction f

        CustomTypeDeclaration d ->
            CustomTypeDeclaration <| noRangeTypeDeclaration d

        PortDeclaration d ->
            PortDeclaration (noRangeSignature d)

        AliasDeclaration aliasDecl ->
            AliasDeclaration (noRangeTypeAlias aliasDecl)

        InfixDeclaration infixDecl ->
            InfixDeclaration infixDecl


noRangeLetDeclaration : Node LetDeclaration -> Node LetDeclaration
noRangeLetDeclaration (Node _ decl) =
    Node emptyRange <|
        case decl of
            LetFunction function ->
                LetFunction (noRangeFunction function)

            LetDestructuring pattern expression ->
                LetDestructuring (noRangePattern pattern) (noRangeExpression expression)


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
    Node emptyRange <|
        case typeAnnotation of
            GenericType x ->
                GenericType x

            Typed (Node _ ( a, b )) c ->
                Typed (Node emptyRange ( a, b )) (List.map noRangeTypeReference c)

            Unit ->
                Unit

            Tupled a ->
                Tupled (List.map noRangeTypeReference a)

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
        | constructors = List.map (unRanged noRangeValueConstructor) x.constructors
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
        , arguments = List.map noRangePattern d.arguments
        , name = unRange d.name
    }


noRangeRecordSetter : RecordSetter -> RecordSetter
noRangeRecordSetter ( a, b ) =
    ( unRange a, unRanged noRangeInnerExpression b )


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
            RecordExpr <| List.map (unRanged noRangeRecordSetter) fields

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression = noRangeExpression lambda.expression
                    , args = List.map noRangePattern lambda.args
                }

        RecordUpdateExpression name updates ->
            RecordUpdateExpression (unRanged identity name) (List.map (unRanged noRangeRecordSetter) updates)

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
            RecordAccess (noRangeExpression e) (unRange n)

        Negation expr ->
            Negation (noRangeExpression expr)

        _ ->
            inner
