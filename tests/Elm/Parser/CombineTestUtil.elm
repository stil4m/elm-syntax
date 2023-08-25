module Elm.Parser.CombineTestUtil exposing (noRangeImport, noRangeInfix, noRangeModule, noRangeSignature, noRangeTypeAlias, noRangeTypeDeclaration, noRangeTypeReference, parseAsFarAsPossible, parseAsFarAsPossibleWithState, parseFullString, parseFullStringState, parseFullStringWithNullState, parseStateToMaybe, pushIndent, unRanged)

import Combine exposing (..)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
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

        Explicit list ->
            list
                |> List.map noRangeExpose
                |> Explicit


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
            GenericType x ->
                GenericType x

            Typed (Node _ ( a, b )) c ->
                Typed (Node.empty ( a, b )) (List.map noRangeTypeReference c)

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


noRangeSignature : Signature -> Signature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation, name = unRange signature.name }
